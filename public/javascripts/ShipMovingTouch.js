ShipMoving = function(x,y) {


	var pos = this.pos = new Vector2(x,y); 
	this.angle = 0; 
	var vel = this.vel = new Vector2(0,0);
	var targetVel = this.targetVel = new Vector2(0,0);  
	var temp = new Vector2(0,0); 
	var name = "Anonymous"

	var color = "rgb(255,255,255)"
	
	var thrustSize = 0; 

	var drawing = true
	
	var canvas = this.canvas = document.createElement("canvas"); 
	
	canvas.width = 60; 
	canvas.height = 60;
	canvas.style = "display:block; position:absolute; background-color:'#ff0000';"; 
	canvas.style.webkitTransformOrigin = canvas.style.MozTransformOrigin = canvas.style.OTransformOrigin = canvas.style.transformOrigin = "30px 30px"; 
	
	var c = canvas.getContext( '2d' );
	this.c = c;  
	 
	var counter = 0; 

	this.updateName = function(n) {
		name = n
	}

	this.updateColor = function(n) {
		color = n
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

		if (drawing == true) {	
			
			c.clearRect(0,0,60,60); 
			//c.fillStyle = "rgba(255,255,255,0.5)";
			c.lineCap = 'square'
	        c.font = "12pt Calibri";
			c.fillStyle = color;
			//c.fillRect(0,0,60,60); 
			c.save();
			c.translate(30, 30); 
			//this.c.rotate(this.angle * Vector2Const.TO_RADIANS);

			//c.strokeStyle = "#fff"; 
			c.strokeStyle = color; 
			c.lineWidth = 2; 
			
			c.beginPath();
			c.fillText(name, -10, -15) 
			c.moveTo(-10, -10);
			c.lineTo(-10, 10);
			c.lineTo(14, 0);
			c.closePath(); 
			c.stroke();
		
			if(thrustSize>0) {

				c.beginPath();
				c.moveTo(-10, -6);
				
				c.lineTo(-10 - (thrustSize/((counter%2)+1)) , 0);
				
				c.lineTo(-10, 6);
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

	this.around = function(vx, vy) {
		var downx = (this.pos.x - 40)
		var upx = (this.pos.x + 40)
		var downy = (this.pos.y - 40)
		var upy = (this.pos.y + 40)
		if (vx > downx && vx < upx && vy > downy  && vy < upy ) {
			this.drawing = false
		}
	}
}; 