ShipMoving = function(x,y) {


	var pos = this.pos = new Vector2(x,y); 
	this.angle = 0; 
	var vel = this.vel = new Vector2(0,0);
	var targetVel = this.targetVel = new Vector2(0,0);  
	var temp = new Vector2(0,0); 
	var name = this.name = "Anonymous";

	var color = "rgb(255,255,255)";

	var thrustSize = 0; 

	var drawing = true;
	
	var canvas = this.canvas = document.createElement("canvas"); 
	
	canvas.width = 60; 
	canvas.height = 60;
	canvas.style = "display:block; position:absolute; background-color:'#ff0000';"; 
	canvas.style.webkitTransformOrigin = canvas.style.MozTransformOrigin = canvas.style.OTransformOrigin = canvas.style.transformOrigin = "30px 30px"; 
	
	var c = canvas.getContext( '2d' );
	this.c = c;  
	 
	var counter = 0; 

	this.updateName = function(n) {
		this.name = n;
	}

	this.setAngle = function(a) {
		this.angle = a;
	}

	this.setThrustSize = function(thrust) {
		thrustSize = thrust;
	}

	this.updateColor = function(n) {
		color = n;
	}

	this.update = function() {
		//speed limit
		var maxSpeed = 30; 
		if(targetVel.isMagGreaterThan(maxSpeed)){
			targetVel.normalise(); 
			targetVel.multiplyEq(maxSpeed); 
			
		}
		if(!targetVel.equals(vel)) {
			
			temp.copyFrom(targetVel); 
			temp.minusEq(vel); 
			if(temp.isMagGreaterThan(0.001)) 
				temp.multiplyEq(0.3); 
		
			vel.plusEq(temp); 
			
		} 
		pos.plusEq(vel);
		
		if(vel.isMagGreaterThan(0)) this.angle = vel.angle();
		 
		//if(thrustSize>0) thrustSize--; 
		thrustSize = vel.magnitude(); 
	};
	

	// c = canvas context
	this.draw = function() {	
		c.clearRect(0,0,60,60); 
		if (drawing) {

			//c.fillStyle = "rgba(255,255,255,0.5)";
			c.lineCap = 'square';
	        c.font = "12pt Calibri";
			c.fillStyle = color;
			//c.fillRect(0,0,60,60);
			c.save();
			c.translate(30, 30);
			//this.c.rotate(this.angle * Vector2Const.TO_RADIANS);

			//c.strokeStyle = "#fff";
			c.strokeStyle = color;
			c.lineWidth = 2;

            // spaceship
			c.beginPath();
            c.fillText(this.name.split("-")[0], -30, -15);
			c.moveTo(-10, -10);
			c.lineTo(-10, 10);
            c.lineTo(0, 4);
            c.lineTo(19, 0);
            c.lineTo(0, -4);

            // space gradient
            var grd = c.createLinearGradient(-10, 0, 19, 0);
            grd.addColorStop(0.67, "#d6d6d6");
            grd.addColorStop(0.37, "#626a94");
            grd.addColorStop(0.10, "#141414");
            c.fillStyle = grd;
            c.fill();

			c.closePath();
			c.stroke();

			if(thrustSize>0) {
				c.beginPath();
              	c.moveTo(-10, -6);
                c.lineTo(-10 - (thrustSize/((counter%2)+1)) , 0);
				c.lineTo(-10, 6);

                // flame
                var grd2 = c.createRadialGradient(-10,-6,10,-10,6 ,0);
                grd2.addColorStop(0, "#efdf2f");
                grd2.addColorStop(0.67, "#d13a3a");
                c.fillStyle = grd2;
                c.fill();

				//c.closePath();
				c.stroke();
				counter++; 
			}
			
			c.restore();
			
			var posx = Math.round(pos.x-30); 
			var posy = Math.round(pos.y-30); 
			
			var styleStr = "translate3d("+posx+"px, "+posy+"px, 0px) rotate("+this.angle+"deg)"; 
			canvas.style.webkitTransform = canvas.style.MozTransform = canvas.style.OTransform = canvas.style.transform = styleStr; 
			//console.log(styleStr); 
		}
	};

	this.around = function(vx, vy, from) {
		if (from == this.name) {
			return false
		}
		var downx = (this.pos.x - 10)
		var upx = (this.pos.x + 10)
		var downy = (this.pos.y - 10)
		var upy = (this.pos.y + 10)
		if (vx > downx && vx < upx && vy > downy  && vy < upy ) {
			//console.log("[" + name + "]I'm dying !!!")
			drawing = false;
			pos.x = -30;
			pos.y = -30;
			return true
		}
		return false
	}
}; 