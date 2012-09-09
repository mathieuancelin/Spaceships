package core

import play.api._
import models._
import akka.actor._
import play.api.libs.json._
import play.api.libs.concurrent._
import play.api.libs.ws._
import play.api.cache._
import play.api.Play.current
import controllers._
import scala.collection.mutable._
import akka.util.duration._
import java.util._
import controllers._
import scala.collection.JavaConversions._
import play.api.libs.json._
import scala.collection.immutable.{ List => JList }

case class Tick()
case class Move(dir: String)
case class Shoot(x: Int, y: Int, dir: String) // from
case class Killed(x: Int, y: Int)// from

class ActorPlayer(name: String, var posX: Int = 0, var posY: Int = 0) extends Actor with ActorLogging {

    // la socket de retour    
    val INC = 5

    def receive = {
        case Move("LEFT")  => {
            if (posX > 0) posX = posX - INC
            push("alive")
        }
        case Move("RIGHT") => {
            if (posX < JUGActors.XMAX) posX = posX + INC
            push("alive")
        }
        case Move("UP")    => {
            if (posY > 0) posY = posY - INC
            push("alive")
        }
        case Move("DOWN")  => {
            if (posY < JUGActors.YMAX) posY = posY + INC 
            push("alive")
        }       
        case Killed(x, y)  => {
            if (x == posX && y == posY)  self ! PoisonPill
            push("killed")
        }
    }

    def push(status: String) = {
        Application.playersEnumerator.push(JsObject(JList(
            "name" -> JsString( name ),
            "x" -> JsNumber( posX ),
            "y" -> JsNumber( posY ),
            "status" -> JsString( status )
        )))
    }
}

class ShootActor extends Actor with ActorLogging {

    // init avec le from

    def receive = {
        case Tick() => {
            var listOfBullets = JList.empty[JsObject]
            for ( shot <- JUGActors.shots ) {
                shot.move()
                if ( shot.done() ) {
                    JUGActors.shots.remove( shot )
                }
                val bullet = JsObject(JList(
                    "name" -> JsString( "agamebullet" ),
                    "x" -> JsString( shot.x + "" ),
                    "y" -> JsString( shot.y + "" ),
                    "status" -> JsString( "bullet" )
                ))
                listOfBullets = listOfBullets :+ bullet
            }
            Application.bulletsEnumerator.push(JsObject(JList(
                "bullets" -> JsArray(listOfBullets)
            )))
        }
        case Shoot(x, y, dir) => JUGActors.shots.add( new Shot(x, y, dir) )
    }
}

class Shot(var x: Int, var y: Int, var dir: String) {
    val INC = 5
    def move() = {
        dir match {
            case "LEFT"  => x = x - INC
            case "RIGHT" => x = x + INC
            case "UP"    => y = y - INC
            case "DOWN"  => y = y + INC
        }
        //for (ref <- players.values) {
        //    ref ! Killed(x, y)
        //}
    }
    def done() = {
        (x, y) match {
            case (xx, _) if xx < 0 => true
            case (xx, _) if xx > JUGActors.XMAX => true
            case (_, yy) if yy < 0 => true
            case (_, yy) if yy > JUGActors.YMAX => true
        }
    }
}

object JUGActors {

    val XMAX = 600
    val YMAX = 600

    val shots = new ArrayList[Shot]

    val shooter = Akka.system.actorOf(Props[ShootActor], name = "shootactor")

    def start() = {
        Akka.system.scheduler.schedule(0 millisecond, 200 milliseconds) {
            shooter ! Tick()
        }
    }
}