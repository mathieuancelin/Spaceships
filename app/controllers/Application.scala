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
import scala.collection.immutable.{ List => JList }

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

    val toEventSource = Enumeratee.map[JsValue] { msg => "data: " + msg + "\n\n" }

    var playersEnumerators = new ConcurrentHashMap[String, PushEnumerator[JsValue]]

    def playerUsername( username: String ) = {
        "playerWithUsername-" + username
    }

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
                val key = playerUsername( username )
                if ( !players.containsKey( key ) ) {
                    val actor = Akka.system.actorOf(Props(new ActorPlayer(username)), name = key)
                    players.putIfAbsent( key, actor)
                }
                Redirect("/mobile/" + username + "/pad")
            } 
        )
    }

    def playersSSE() = Action { implicit request =>
        Ok.feed( playersHub.getPatchCord().through( toEventSource ) ).as( "text/event-stream" )
    }

    def bulletSSE() = Action { implicit request =>
        Ok.feed( bulletsHub.getPatchCord().through( toEventSource ) ).as( "text/event-stream" )
    }

    def mobilePadStream( username: String ) = WebSocket.async[JsValue] { request =>
        var out = createUserIfAbsent( username )
        var in = Iteratee.foreach[JsValue] ( _ match {
            case message: JsObject => {
                //println( message )
                playersEnumerator.push( message )
                /**val key = playerUsername( username )
                if ( players.containsKey( key ) ) {
                    val actor = players.get( key )
                    ( message \ "command" ).as[String] match {
                        case "MOVE" => actor ! Move( ( message \ "dir" ).as[String] )
                        case "SHOOT" => JUGActors.shooter ! Shoot( ( message \ "x" ).as[Int], 
                            ( message \ "y" ).as[Int], ( message \ "dir" ).as[String] )
                    }
                }**/
            }
            case _ =>
        })
        Promise.pure( ( in, out ) )
    }

    def padAction( username: String ) = Action { implicit request =>
        createUserIfAbsent( username )
        actionForm.bindFromRequest.fold (
            formWithErrors => BadRequest,
            { action =>
                playersEnumerator.push( Json.parse( action ) )
                Ok
            } 
        )
    }

    def createUserIfAbsent( username: String ) = {
        if ( !playersEnumerators.containsKey( username ) ) {
            playersEnumerators.put( username, Enumerator.imperative[JsValue]( ) )
        } 
        playersEnumerators.get( username )
    }

    def kill( username: String ) = Action { implicit request =>
        val out = Option( playersEnumerators.get( username ) )
        out.map { player =>
            println( "sending kill to " + username)
            player.push( JsObject( JList( "action" -> JsString( "kill" ) ) ) )
            playersEnumerators.remove( username )
        }
        Ok
    }
}