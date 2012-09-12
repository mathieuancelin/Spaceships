package controllers

import play.api._
import play.api.mvc._
import play.api.data.Forms._
import play.api.data._

import play.api.Play.current
import play.api.libs._
import play.api.libs.iteratee._
import play.api.libs.concurrent._
import java.util.concurrent._
import scala.concurrent.stm._
import akka.util.duration._
import play.api.cache._
import play.api.libs.json._
import core._
import akka.actor._
import java.util.concurrent.ConcurrentHashMap
import scala.collection.immutable.{ List => JList }

object Application extends Controller {

    val usernameForm = Form( "username" -> text )  
    val actionForm = Form( "message" -> text )  

    val sizeForm = Form( tuple( "width" -> text, "height" -> text ) )  

    val playersEnumerator = Enumerator.imperative[JsValue]( )
    val bulletsEnumerator = Enumerator.imperative[JsValue]( )

    val playersHub = Concurrent.hub[JsValue]( playersEnumerator )
    val bulletsHub = Concurrent.hub[JsValue]( bulletsEnumerator )

    var sinkEnumerator = Enumerator.imperative[JsValue]( )
    var sinkIteratee = Iteratee.foreach[JsValue] ( _ match { case _ => } )

    var currentGame = Option( Game( playersEnumerator ) )

    def index() = Action { implicit request =>
        currentGame.map { game =>
            game.stop()
        }
        currentGame = Option( Game( playersEnumerator ) )
        Ok( views.html.board() )    
    }

    def mobileStart() = Action { implicit request =>
        Ok( views.html.mobilestart() )
    }

    def mobilePad(username: String) = Action { implicit request =>
        Ok( views.html.control( username ) )
    }

    def startGame() = Action { implicit request =>
        usernameForm.bindFromRequest.fold (
            formWithErrors => BadRequest( "You need to post a 'username' value!" ),
            { username =>
                Redirect("/mobile/" + username.replace(" ", "") + "-" + System.nanoTime() + "/pad")
            } 
        )
    }

    def playersSSE() = Action { implicit request =>
        Ok.feed( playersHub.getPatchCord().through( EventSource() ) ).as( "text/event-stream" )
    }

    def bulletsSSE() = Action { implicit request =>
        Ok.feed( bulletsHub.getPatchCord().through( EventSource() ) ).as( "text/event-stream" )
    }

    // for websocket capable devices
    def mobilePadStream( username: String ) = WebSocket.async[JsValue] { request =>
        currentGame.map { game =>
            var out = game.createUser( username )
            game.pushWaitingList( playersEnumerator )
            var in = Iteratee.foreach[JsValue] ( _ match {
                case message: JsObject => {
                    processInputFromPlayer( username, message )
                }
                case _ => // do nothing
            })
            Promise.pure( ( in, out ) )
        }.getOrElse( 
            Promise.pure( ( sinkIteratee, sinkEnumerator ) ) 
        )
    }

    // for non websocket capable devices
    def padAction( username: String ) = Action { implicit request =>
        currentGame.map { game =>
            game.createUser( username )
            //game.pushWaitingList( playersEnumerator )
            actionForm.bindFromRequest.fold (
                formWithErrors => BadRequest( "You have to provide an 'action' value." ),
                { action =>
                    processInputFromPlayer( username, Json.parse( action ) )
                    Ok
                } 
            )
        }.getOrElse( 
            InternalServerError( "There is currently no game running" ) 
        )
    }

    def killAction( username: String ) = Action { implicit request =>
        currentGame.map { game =>
            Ok( game.kill( username ) )
        }.getOrElse( 
            InternalServerError( "There is currently no game running" ) 
        )
    }

    def defineCanvasSize() = Action { implicit request =>
        sizeForm.bindFromRequest.fold (
            formWithErrors => BadRequest( "You need to post 'width' and 'height' values!" ),
            { form =>
                currentGame.map { game =>
                    game.XMAX = Integer.valueOf(form._1)
                    game.YMAX = Integer.valueOf(form._2)
                }
                Ok
            } 
        )
    }

    def processInputFromPlayer( username: String, message: JsValue) = {
        currentGame.map { game =>
            val key = Game.playerUsername( username )
            if ( game.activePlayers.containsKey( username ) ) {
                val actor = game.activePlayers.get( username ).actor
                ( message \ "action" ).as[String] match {
                    case "moving" => actor ! Move( ( message \ "x" ).as[Double],  ( message \ "y" ).as[Double] )
                    case "fire" => actor ! Shoot( ( message \ "x" ).as[Double],  ( message \ "y" ).as[Double] )
                    case _ =>
                }
            }
        }
    }
}