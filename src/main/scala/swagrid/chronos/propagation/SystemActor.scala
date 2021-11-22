package swagrid.chronos.propagation

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.LoggerOps
import akka.actor.typed.{ ActorRef, ActorSystem, Behavior }

import swagrid.chronos.structures.*
import swagrid.chronos.reactives.*

object SystemActor:
  
  /*def apply(system: System): Behavior[EventTrigger[_]] =
    Behaviors.setup { context =>
    }*/
    
  case class EventTrigger[T](event: Event[T], value: T)