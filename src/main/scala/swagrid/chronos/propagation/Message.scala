package swagrid.chronos.propagation

import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import swagrid.chronos.reactives.*
import swagrid.chronos.structures.*

import swagrid.chronos.propagation.ReactiveState.*

sealed trait Message

object Message:
  
  case class Subscribe (
    subscriber: Contact[_, _],
    mask: TemporalMask
  ) extends Message
  
  case class PushSignal[T] (
    publisher: Contact[T, Signal],
    update: SignalValue[T]
  ) extends Message
  
  case class PushEvent[T] (
    publisher: Contact[T, Event],
    update: EventValue[T]
  ) extends Message
  
  case class Pull (
    mask: TemporalMask
  ) extends Message