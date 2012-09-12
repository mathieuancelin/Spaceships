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
case class Shoot(x: Double, y: Double)
case class Kill(x: Double, y: Double)

class ActorPlayer( name: String, var posX: Double = 300.0, var posY: Double = 300.0, 
    spaceShip: SpaceShip, currentGame: Option[Game] ) extends Actor with ActorLogging {

    //val spaceShip = new SpaceShip( posX, posY )

    val game = currentGame.get

    var alive = true

    def receive = {
        case Move( x, y ) => {
            val oldX = posX
            val oldY = posY
            spaceShip.targetVel.copyFromXY( x, y )
            spaceShip.targetVel.multiplyEq( 0.2 )
            spaceShip.update()
            if ( spaceShip.pos.x < 1 ) {
                spaceShip.pos.x = game.XMAX
            } else if ( spaceShip.pos.x > game.XMAX) {
                spaceShip.pos.x = 1
            }
            if ( spaceShip.pos.y < 1) {
                spaceShip.pos.y = game.YMAX
            } else if ( spaceShip.pos.y > game.YMAX ) {
                spaceShip.pos.y = 1
            }
            posX = spaceShip.pos.x
            posY = spaceShip.pos.y
            //if ( !(posX == oldX && posY == oldY) ) {
                push( "alive", "moving", spaceShip )
            //}
        }
        case Shoot( x, y) => {
            val bullet = Bullet(name, spaceShip.pos.x, spaceShip.pos.y, spaceShip.angle)
            bullet.vel.plusEq( spaceShip.vel ) 
            push( "alive", "fire", spaceShip )
            /**currentGame.map { game =>
                val act = game.system.actorOf(Props(new BulletActor(bullet, currentGame)), name = "bullet-" + bullet.from + "-" + bullet.id)
                game.system.eventStream.subscribe(act, classOf[Tick])
                //game.shooter ! bullet
            }**/
        }
        case Kill( x, y) => {
            //if  ( spaceShip.around(x, y) ) {
            //println( "[" + name + "] I'm dead bro !")
            alive = false
            /**currentGame.map { game =>
                game.kill( name )
            }**/
            //}
        }
    }

    def push( status: String, action: String, spaceShip: SpaceShip ) = {
        if (alive) {
            Application.playersEnumerator.push(JsObject(JList(
                "name" -> JsString( name ),
                "action" -> JsString( action ),
                "angle" -> JsNumber( spaceShip.angle ),
                "thrust" -> JsNumber( spaceShip.thrustSize ),
                "x" -> JsNumber( posX ),
                "y" -> JsNumber( posY ),
                "velx" -> JsNumber( spaceShip.vel.x ),
                "vely" -> JsNumber( spaceShip.vel.y )
            )))
        }
    }
}

class BulletActor( bullet: Bullet, currentGame: Option[Game] ) extends Actor with ActorLogging {
    def receive = {
        case Tick() => {
            bullet.update()
            if (bullet.enabled) {
                Application.bulletsEnumerator.push( JsObject(JList(
                    "action" -> JsString( "shoot" ),
                    "id" -> JsString( bullet.id ),
                    "x" -> JsNumber( bullet.pos.x ),
                    "y" -> JsNumber( bullet.pos.y ),
                    "disabled" -> JsString( "false" )
                )))
                currentGame.map { game =>
                    for ( player <- game.activePlayers.values() ) {
                        if (!player.username.equals( bullet.from )) {
                            if (player.spaceShip.around( bullet.pos.x, bullet.pos.y )) {
                                //println( "[" + player.username + "] I'm dead bro !")
                                game.kill( player.username )
                                player.actor ! Kill( bullet.pos.x, bullet.pos.y )
                            }
                        }
                    }
                }
            } else {
                Application.bulletsEnumerator.push( JsObject(JList(
                    "action" -> JsString( "shoot" ),
                    "id" -> JsString( bullet.id ),
                    "disabled" -> JsString( "true" )
                )))
                currentGame.map { game =>
                    game.system.eventStream.unsubscribe( self )
                }
                self ! PoisonPill
            }
        }
    }
}

class ShootActor( currentGame: Option[Game] ) extends Actor with ActorLogging {

    var bullets = JList[Bullet]()

    def receive = {
        case Tick() => {
            currentGame.map { game =>
                bullets = bullets.filter { bullet =>
                    bullet.update()
                    if (bullet.enabled) {
                        Application.bulletsEnumerator.push( JsObject(JList(
                            "action" -> JsString( "shoot" ),
                            "id" -> JsString( bullet.id ),
                            "x" -> JsNumber( bullet.pos.x ),
                            "y" -> JsNumber( bullet.pos.y ),
                            "disabled" -> JsString( "false" )
                        )))
                        for ( player <- game.activePlayers.values() ) {
                            if (!player.username.equals( bullet.from )) {
                                if (player.spaceShip.around( bullet.pos.x, bullet.pos.y )) {
                                    //println( "[" + player.username + "] I'm dead bro !")
                                    game.kill( player.username )
                                    player.actor ! Kill( bullet.pos.x, bullet.pos.y ) 
                                }
                            }
                        }
                    } else {
                        Application.bulletsEnumerator.push( JsObject(JList(
                            "action" -> JsString( "shoot" ),
                            "id" -> JsString( bullet.id ),
                            "disabled" -> JsString( "true" )
                        )))
                    }
                    bullet.enabled
                }
            }
        }
        case bullet: Bullet => bullets = bullets.:+( bullet )
    }
}