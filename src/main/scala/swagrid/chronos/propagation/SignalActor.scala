package swagrid.chronos.propagation

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.LoggerOps
import akka.actor.typed.{ ActorRef, ActorSystem, Behavior }

import swagrid.chronos.structures.*
import swagrid.chronos.reactives.*
import swagrid.chronos.propagation.ReactiveState.*

object SignalActor:
  
  def apply[T](signal: Signal[T]): Behavior[Message] =
    signalBehavior(signal, ReactiveState(SignalValue.unknown))
  
  private def signalBehavior[T](signal: Signal[T], state: SignalState[T]): Behavior[Message] =
    
    Behaviors.receive { (context, message) =>
      
      val self = Contact(signal, context.self)
      
      message match
      
        case Message.Subscribe(subscriber, mask) =>
          
          val diff = state.history.signalValue(signal).crop(mask)
          if !diff.isEmpty then
            subscriber.actor ! Message.PushSignal(self, diff)
            
          val updated = state.updateChild(subscriber)(_ => mask - diff.mask)
          signalBehavior(signal, updated)
          
        case Message.PushSignal(publisher, update) =>
          
          val updated = state
            .updateParentValues(_.updateSignal(publisher.reactive, update))
            .updateParent(publisher)(m => m - update.mask)
          
          signalBehavior(signal, updateValue(self, updated))
  
        case Message.PushEvent(publisher, update) =>
  
          val updated = state
            .updateParentValues(_.updateEvent(publisher.reactive, update))
            .updateParent(publisher)(m => m - update.mask)
  
          signalBehavior(signal, updateValue(self, updated))
  
        case Message.Pull(mask) =>
          
          val diff = mask - state.pullMask
          if !diff.isEmpty then
            state.parents.foreach { (parent, mask) =>
              parent.actor ! Message.Pull(diff & mask)
            }
          val updated = state.updatePullMask(m => m | mask)
          signalBehavior(signal, updated)
    }
    
  private def updateValue[T](self: Contact[T, Signal], state: SignalState[T]): SignalState[T] =
    
    val givens = state.history.updateSignal(self.reactive, state.value)
    val updated = self.reactive.update(givens)
    val diff = updated.crop(!state.value.mask)
    val safeToRemove = updated.mask.trimLeft & state.value.mask
  
    state.children.foreach { (child, mask) =>
      val update = diff.crop(mask)
      if !update.isEmpty then
        child.actor ! Message.PushSignal(self, update)
    }
    
    state
      .updateValue(_ => updated)
      .updateParentValues(_.crop(!safeToRemove))
      .updateChildren(m => m - diff.mask)
      .updatePullMask(m => m & updated.mask)