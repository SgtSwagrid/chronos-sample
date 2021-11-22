package swagrid.chronos.propagation

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.LoggerOps
import akka.actor.typed.{ ActorRef, ActorSystem, Behavior }

import swagrid.chronos.structures.*
import swagrid.chronos.reactives.*
import swagrid.chronos.propagation.ReactiveState.*

object EventActor:
  
  def apply[T](event: Event[T]): Behavior[Message] =
    eventBehavior(event, ReactiveState(EventValue.empty))
  
  private def eventBehavior[T](event: Event[T], state: EventState[T]): Behavior[Message] =
    
    Behaviors.receive { (context, message) =>
      
      val self = Contact(event, context.self)
      
      message match
        
        case Message.Subscribe(subscriber, mask) =>
          
          val diff = state.history.eventValue(event).crop(mask)
          if !diff.isEmpty then
            subscriber.actor ! Message.PushEvent(self, diff)
          
          val updated = state.updateChild(subscriber)(_ => mask - diff.mask)
          eventBehavior(event, updated)
        
        case Message.PushSignal(publisher, update) =>
          
          val updated = state
            .updateParentValues(_.updateSignal(publisher.reactive, update))
            .updateParent(publisher)(m => m - update.mask)
  
          eventBehavior(event, updateValue(self, updated))
        
        case Message.PushEvent(publisher, update) =>
          
          val updated = state
            .updateParentValues(_.updateEvent(publisher.reactive, update))
            .updateParent(publisher)(m => m - update.mask)
  
          eventBehavior(event, updateValue(self, updated))
        
        case Message.Pull(mask) =>
          
          val diff = mask - state.pullMask
          if !diff.isEmpty then
            state.parents.foreach { (parent, mask) =>
              parent.actor ! Message.Pull(diff & mask)
            }
          val updated = state.updatePullMask(m => m | mask)
          eventBehavior(event, updated)
    }
  
  private def updateValue[T](self: Contact[T, Event], state: EventState[T]): EventState[T] =
    
    val givens = state.history.updateEvent(self.reactive, state.value)
    val updated = self.reactive.update(givens)
    val diff = updated.crop(!state.value.mask)
    
    state.children.foreach { (child, mask) =>
      val update = diff.crop(mask)
      if !update.isEmpty then
        child.actor ! Message.PushEvent(self, update)
    }
    
    state
      .updateValue(_ => updated)
      .updateParentValues(_.crop(!updated.mask))
      .updateChildren(m => m - diff.mask)
      .updatePullMask(m => m & updated.mask)