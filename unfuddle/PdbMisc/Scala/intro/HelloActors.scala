import scala.actors.Actor

abstract class ActorMessage
case class Greeting() extends ActorMessage
case class Quit() extends ActorMessage

class MovieActor extends Actor {
  private var acting = true

  def act() {
    while (acting) {
      receive {
        case Quit => acting = false
      }
    }    
  }
}

object HelloActors {
  def main(args: Array[String]) {
    val sean = new MovieActor;
    sean.run();
    sean ! Quit;
  }
}
