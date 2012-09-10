import play.api._
import play.api.mvc._
import core._

object Global extends GlobalSettings {

    override def onStart( app: Application ) {

    }

    override def onStop( app: Application ) {
    	controllers.Application.currentGame.map { game =>
    		game.stop()
    	}
    }
}