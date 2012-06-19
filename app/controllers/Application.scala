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
import utils._
import akka.actor._
import java.util.concurrent.ConcurrentHashMap

object Application extends Controller {

    val XMAX = 1000
    val YMAX = 500

    val usernameForm = Form( "username" -> text )  
    val actionForm = Form( "message" -> text )  

    val playersEnumerator = Enumerator.imperative[JsValue]( )
    val bulletsEnumerator = Enumerator.imperative[JsValue]( )
    val playersHub = Concurrent.hub[JsValue]( playersEnumerator )
    val bulletsHub = Concurrent.hub[JsValue]( bulletsEnumerator )
    val players = new ConcurrentHashMap[String, ActorRef]

    def index() = Action { implicit request =>
        Ok( views.html.board() )    
    }

    def mobileStart() = Action { implicit request =>
        Ok( views.html.mobilestart() )
    }

    /**def mobilePad(username: String) = Action { implicit request =>
        Ok( views.html.mobilepad(username) )
    }**/

    def mobilePad(username: String) = Action { implicit request =>
        Ok( views.html.control( username ) )
    }

    def startGame() = Action { implicit request =>
        usernameForm.bindFromRequest.fold (
            formWithErrors => BadRequest( "You need to post a 'username' value!" ),
            { username =>
                val key = "playerWithUsername" + username
                val actor = Akka.system.actorOf(Props(new ActorPlayer(username)), name = "playerWithUsername" + username)
                if ( !players.containsKey( key ) ) {
                    players.putIfAbsent( key, actor)
                }
                Redirect("/mobile/" + username + "/pad")
            } 
        )
    }

    def playersSSE() = Action { implicit request =>
        SimpleResult(
            header = ResponseHeader(
                OK, Map( CONTENT_LENGTH -> "-1", CONTENT_TYPE -> "text/event-stream" )
            ), playersHub.getPatchCord().through( Enumeratee.map[JsValue] { msg => "data: " + msg + "\n\n" } )
        )
    }

    def bulletSSE() = Action { implicit request =>
        SimpleResult(
            header = ResponseHeader(
                OK, Map( CONTENT_LENGTH -> "-1", CONTENT_TYPE -> "text/event-stream" )
            ), bulletsHub.getPatchCord().through( Enumeratee.map[JsValue] { msg => "data: " + msg + "\n\n" } )
        )
    }

    def mobilePadStream( username: String ) = WebSocket.async[JsValue] { request =>
        var out = Enumerator.imperative[JsValue]( )
        var in = Iteratee.foreach[JsValue] ( _ match {
            case message: JsObject => {
                //println( message )
                playersEnumerator.push( message )
                /**val key = "playerWithUsername" + username
                if ( players.containsKey( key ) ) {
                    val actor = players.get( key )
                    ( message \ "command" ).as[String] match {
                        case "MOVE" => actor ! Move( ( message \ "dir" ).as[String] )
                        case "SHOOT" => JUGActors.shooter ! Shoot( ( message \ "x" ).as[Int], 
                            ( message \ "y" ).as[Int], ( message \ "dir" ).as[String] )
                    }
                }**/
            }
        })
        Promise.pure( ( in, out ) )
    }

    def padAction( username: String ) = Action { implicit request =>
        actionForm.bindFromRequest.fold (
            formWithErrors => BadRequest,
            { action =>
                playersEnumerator.push( Json.parse( action ) )
                Ok
            } 
        )
    }
}