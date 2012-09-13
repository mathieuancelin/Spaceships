package core

import play.api.Play.current
import play.api.libs._
import play.api.libs.iteratee._
import play.api.libs.concurrent._
import java.util.concurrent._
import scala.concurrent.stm._
import akka.util.duration._
import play.api.libs.json._
import akka.actor._
import java.util._
import java.util.concurrent.ConcurrentHashMap
import scala.collection.immutable.{ List => JList }
import controllers._
import scala.collection.JavaConversions._

case class Player( username: String, spaceShip: SpaceShip, enumerator: PushEnumerator[JsValue], actor: ActorRef )

class Game( enumerator: PushEnumerator[JsValue] ) {

    val system = ActorSystem("CurrentGameSystem")

    var activePlayers = new ConcurrentHashMap[String, Player]

    var waitingPlayers = new ConcurrentHashMap[String, Player]

    var waitingPlayersName = new ArrayList[String]

    val shooter = system.actorOf(Props(new ShootActor(Option(this))), name = "currentshootactor")

    var XMAX = 600
    var YMAX = 1000

    def start() = {
        /**system.scheduler.schedule(0 millisecond, 28 milliseconds) {
            //shooter ! Tick()
            system.eventStream.publish( Tick() )  
        }**/
    }

    def stop() = {
        shooter ! PoisonPill
        system.shutdown()
    }

    def createUser( username: String ):PushEnumerator[JsValue] = {
        if ( activePlayers.size < Game.playerMax) { 
            createUserIfAbsent( username, "play", activePlayers )
        } else {
            if (activePlayers.containsKey()) {
                return activePlayers.get( username ).enumerator
            } 
        	if ( !waitingPlayersName.contains( username ) ) {
        		waitingPlayersName.add( username )
        	}
            createUserIfAbsent( username, "wait", waitingPlayers ) 
        }
    }

    def createUserIfAbsent( username: String, action: String, map: ConcurrentHashMap[String, Player] ) = {
    	if ( !map.containsKey( username ) ) {
            val key = Game.playerUsername( username )
            val ship = new SpaceShip( 300, 300 )
            val actor = system.actorOf(Props(new ActorPlayer(username, 
                spaceShip = ship, currentGame = Option( this ))), name = key)
            map.put( username, Player( username, ship, Enumerator.imperative[JsValue]( ), actor ) )
        } 
        val pushEnum = map.get( username ).enumerator
        system.scheduler.scheduleOnce(200 milliseconds) {
            pushEnum.push( JsObject( JList( "action" -> JsString( action ) ) ) )
        }
        pushEnum
    }

    def kill( username: String ) = {
    	val out = Option( activePlayers.get( username ) )
        out.map { player =>
            player.enumerator.push( JsObject( JList( "action" -> JsString( "kill" ) ) ) )
            enumerator.push( JsObject( JList( "action" -> JsString( "kill" ), "name" -> JsString( username ) ) ) )
            player.actor ! Kill( 0, 0 )
            player.actor ! PoisonPill
            activePlayers.remove( username )
            if (!waitingPlayers.isEmpty()) {
                val waitingPlayer = waitingPlayers.get( waitingPlayersName.iterator().next() )
                waitingPlayers.remove( username )
                waitingPlayersName.remove( username )
                activePlayers.put( waitingPlayer.username, waitingPlayer )
                waitingPlayer.enumerator.push( JsObject( JList( "action" -> JsString( "play" ) ) ) )
            }
        }
        pushWaitingList( Application.playersEnumerator )
        if (out.isDefined && activePlayers.isEmpty() && waitingPlayers.isEmpty()) {
            Application.currentGame = None
            "nowinner"
        } else 
        if (out.isDefined && activePlayers.size == 1 && waitingPlayers.isEmpty()) {
        //if (activePlayers.size == 1 && waitingPlayers.isEmpty()) {
            // stop game and call winner
            val p = activePlayers.entrySet().iterator().next().getValue()
            p.enumerator.push( JsObject( JList( "action" -> JsString( "win" ) ) ) )
            Application.currentGame = None
            "winner:" + p.username
        } else { 
            "continue"
        }
    }

    def pushWaitingList( enumerator: PushEnumerator[JsValue] ) = {
        var waiting = JList[JsObject]( )
        for ( player <- waitingPlayers.values() ) {
            waiting = waiting :+ JsObject( JList( "player" -> JsString( player.username ) ) )
        }
        enumerator.push( JsObject( JList( "action" -> JsString( "waitinglist" ), "players" -> JsArray( waiting ) ) ) )
    }
}

object Game {

    val playerMax = 50

    val anon = "Anon"

    var counter = 0

    def playerUsername( username: String ) = {
        "playerWithUsername-" + username
    }

    def apply( enumerator: PushEnumerator[JsValue] ): Game = {
        val game = new Game( enumerator ) 
        game
    }

    def sanitizeUsername( username: String) = {
        val sane = username.replace(" ", "").replace("-", "").replaceAll("[^a-zA-Z0-9]", "").trim()
        if (sane.isEmpty()) {
            counter = counter + 1
            anon + counter
        } else {
            sane
        }
    }
}