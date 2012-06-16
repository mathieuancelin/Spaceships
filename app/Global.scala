import play.api._
import play.api.mvc._
import utils._

object Global extends GlobalSettings {

    override def onStart( app: Application ) {
        JUGActors.start()
    }
}