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
case class Move(x: Double, y: Double)
case class Shoot(x: Int, y: Int, dir: String) // from
case class Killed(x: Int, y: Int)// from

class ActorPlayer( name: String, var posX: Double = 300.0, var posY: Double = 300.0 ) extends Actor with ActorLogging {

    val spaceShip = new SpaceShip( posX, posY )

    def receive = {
        case Move( x, y ) => {
            spaceShip.targetVel.copyFromXY( x, y )
            spaceShip.targetVel.multiplyEq( 0.2 )
            spaceShip.update()
            if ( spaceShip.pos.x < 1 ) {
                spaceShip.pos.x = Game.XMAX
            } else if ( spaceShip.pos.x > Game.XMAX) {
                spaceShip.pos.x = 1
            }
            if ( spaceShip.pos.y < 1) {
                spaceShip.pos.y = Game.YMAX
            } else if ( spaceShip.pos.y > Game.YMAX ) {
                spaceShip.pos.y = 1
            }
            posX = spaceShip.pos.x
            posY = spaceShip.pos.y
            push( "alive", spaceShip.angle, spaceShip.thrustSize )
        }
    }

    def push(status: String, angle: Double, thrustSize: Double ) = {
        Application.playersEnumerator.push(JsObject(JList(
            "name" -> JsString( name ),
            "action" -> JsString( "moving" ),
            "angle" -> JsNumber( angle ),
            "thrust" -> JsNumber( thrustSize ),
            "x" -> JsNumber( posX ),
            "y" -> JsNumber( posY )
        )))
    }
}

/**
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
} **/

object JUGActors {

    /**
    val shots = new ArrayList[Shot]

    val shooter = Akka.system.actorOf(Props[ShootActor], name = "shootactor")

    def start() = {
        Akka.system.scheduler.schedule(0 millisecond, 200 milliseconds) {
            shooter ! Tick()
        }
    }**/
}