package core

import scala.math._

class Bullet( var id: String, var from: String, var x: Double, var y: Double, var angle: Double) {
	
	var speed = 10.0
	var pos = new Vector( x, y )
	var vel = new Vector( 1, 0 )
	var life = 50
	var enabled = true

	
	def update() = {
		pos.plusEq(this.vel) 
		life = life - 1
		if( life < 0 ) {
			enabled = false;
		}
	}
	
	def reset( x: Double, y: Double, angle: Double ) = {
		pos = new Vector( x, y )
		val unitv = new Vector( 1, 0 )
		unitv.rotate( angle, false ) 
		vel = unitv.cloneVector()
		vel.multiplyEq( speed )
		unitv.multiplyEq( 10 )
		pos.plusEq(unitv)
		enabled = true 
		life = 50
	}
}

object Bullet {

	def apply( from: String, x: Double, y: Double, angle: Double ): Bullet = {
		val bullet = new Bullet( System.nanoTime() + "", from, x, y, angle )
		bullet.reset( x, y, angle )
		bullet
	}
}

class SpaceShip( var x: Double, var y: Double ) {

	var pos = new Vector( x, y ) 
	var angle = 0.0 
	var vel = new Vector( 0, 0 )
	var targetVel = new Vector( 0, 0 )  
	var temp = new Vector( 0, 0 ) 
	var thrustSize = 0.0

	def update() = {
		//speed limit
		val maxSpeed = 20
		if ( targetVel.isMagGreaterThan( maxSpeed ) ){
			targetVel.normalise()
			targetVel.multiplyEq( maxSpeed )
		}
		if ( !targetVel.equals( vel ) ) {
			temp.copyFrom( targetVel ) 
			temp.minusEq( vel ) 
			if ( temp.isMagGreaterThan( 0.001 ) ) {
				temp.multiplyEq( 0.3 )
			}
			vel.plusEq(temp)
		} 
		pos.plusEq(vel)
		if ( vel.isMagGreaterThan( 0 ) ) {
			angle = vel.angle( false )
		}
		thrustSize = vel.magnitude() 
	}

	def around( vx: Double, vy: Double ):Boolean = {
		var downx = pos.x - 10
		var upx = pos.x + 10
		var downy = pos.y - 10
		var upy = pos.y + 10
		if (vx > downx && vx < upx && vy > downy  && vy < upy ) {
			pos.x = -30;
			pos.y = -30;
			return true
		}
		return false
	}
}

class Vector(var x: Double = 0, var y: Double = 0) {

	def reset( xx: Double, yy: Double ) = {
		x = xx
		y = yy
		this
	}

	def toString( decPlaces: Double = 3 ) = {
		val scalar = pow( 10, decPlaces ) 
		"[" + round (x * scalar) / scalar + ", " + round (this.y * scalar) / scalar + "]"
	}
	
	def cloneVector() = {
		new Vector(x, y)
	}
	
	def copyTo( v: Vector ) = {
		v.x = x
		v.y = y
	}
	
	def copyFrom( v: Vector ) = {
		x = v.x
		y = v.y
	}

	def copyFromXY( xx: Double, yy: Double ) = {
		x = xx
		y = yy
	}	
	
	def magnitude() = {
		sqrt( ( x * x ) + ( y * y ) )
	}
	
	def magnitudeSquared() = {
		( x * x) + ( y * y )
	}
	
	def normalise() = {
		val m = magnitude()
		x = x / m
		y = y / m
		this	
	}
	
	def reverse() = {
		x = -x
		y = -y
		this
	}
	
	def plusEq( v: Vector ) = {
		x += v.x
		y += v.y
		this
	}
	
	def plusNew( v: Vector ) = {
		new Vector( x + v.x, y + v.y ) 
	}
	
	def minusEq( v: Vector ) = {
		x -= v.x
		y -= v.y
		this
	}

	def minusNew( v: Vector ) = {
	 	new Vector( x - v.x, y - v.y ) 
	}	
	
	def multiplyEq( scalar: Double ) = {
		x *= scalar
		y *= scalar
		this
	}
	
	def multiplyNew( scalar: Double ) = {
		val returnvec = cloneVector()
		returnvec.multiplyEq( scalar )
	}
	
	def divideEq( scalar: Double ) = {
		x /= scalar
		y /= scalar
		this 
	}
	
	def divideNew( scalar: Double ) = {
		val returnvec = cloneVector()
		returnvec.divideEq( scalar )
	}

	def dot( v: Vector ) = {
		(x * v.x) + (y * v.y)
	}
	
	def angle( useRadians: Boolean ) = {
		val mult = useRadians match {
			case true => 1
			case false => VectorConst.toDegrees
		}
		atan2(this.y,this.x) * mult
	}
	
	def rotate( angle: Double, useRadians: Boolean ) = {
		val mult = useRadians match {
			case true => 1
			case false => VectorConst.toRadians
		}
		val cosRY = cos(angle * mult)
		val sinRY = sin(angle * mult)
		VectorConst.temp.copyFrom(this)
		x = ( VectorConst.temp.x * cosRY ) - ( VectorConst.temp.y * sinRY )
		y = ( VectorConst.temp.x * sinRY ) + ( VectorConst.temp.y * cosRY )
		this;
	}	
		
	def equals( v: Vector ) = {
		( x == v.x ) && ( y == v.x )
	}
	
	def isCloseTo( v: Vector, tolerance: Double ):Boolean = {	
		if( equals( v ) ) {
			return true
		}
		VectorConst.temp.copyFrom(this)
		VectorConst.temp.minusEq(v) 
		VectorConst.temp.magnitudeSquared() < ( tolerance * tolerance )
	}
	
	def rotateAroundPoint( point: Vector, angle: Double, useRadians: Boolean) = {
		VectorConst.temp.copyFrom(this)
		VectorConst.temp.minusEq(point)
		VectorConst.temp.rotate(angle, useRadians)
		VectorConst.temp.plusEq(point)
		copyFrom(VectorConst.temp)
	} 
	
	def isMagLessThan( distance: Double ) = {
		magnitudeSquared() < ( distance * distance )
	}
	
	def isMagGreaterThan( distance: Double ) = {
		magnitudeSquared() > ( distance * distance )
	}
}

object VectorConst {

	val toDegrees = 180 / Pi	
	val toRadians = Pi / 180
	val temp = new Vector()
}