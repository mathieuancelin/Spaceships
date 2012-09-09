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

    val playersEnumerator = Enumerator.imperative[JsValue]( )
    val bulletsEnumerator = Enumerator.imperative[JsValue]( )
    val playersHub = Concurrent.hub[JsValue]( playersEnumerator )
    val bulletsHub = Concurrent.hub[JsValue]( bulletsEnumerator )

    var currentGame = new Game()

    def index() = Action { implicit request =>
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
                Redirect("/mobile/" + username + "/pad")
            } 
        )
    }

    def playersSSE() = Action { implicit request =>
        Ok.feed( playersHub.getPatchCord().through( EventSource() ) ).as( "text/event-stream" )
    }

    // not used yet
    def bulletSSE() = Action { implicit request =>
        Ok.feed( bulletsHub.getPatchCord().through( EventSource() ) ).as( "text/event-stream" )
    }

    // for websocket capable devices
    def mobilePadStream( username: String ) = WebSocket.async[JsValue] { request =>
        var out = currentGame.createUser( username )
        var in = Iteratee.foreach[JsValue] ( _ match {
            case message: JsObject => {
                // TODO : use game engine instead of relying on client side
                playersEnumerator.push( message )
            }
            case _ => // do nothing
        })
        Promise.pure( ( in, out ) )
    }

    // for non websocket capable devices
    def padAction( username: String ) = Action { implicit request =>
        currentGame.createUser( username )
        actionForm.bindFromRequest.fold (
            formWithErrors => BadRequest,
            { action =>
                playersEnumerator.push( Json.parse( action ) )
                Ok
            } 
        )
    }

    // TODO : remove when game engine will be implemented
    def kill( username: String ) = Action { implicit request =>
        Ok( currentGame.kill( username ) )
    }
}