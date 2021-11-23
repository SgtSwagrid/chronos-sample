package swagrid.chronos.propagation

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.LoggerOps
import akka.actor.typed.{ ActorRef, ActorSystem, Behavior }

import swagrid.chronos.reactives.*
import swagrid.chronos.structures.*
import swagrid.chronos.propagation.ReactiveState.*

case class ReactiveState[T, R[_], V[_]] (
  value: V[T],
  history: History = History(),
  children: Children = Map.empty,
  parents: Children = Map.empty,
  pullMask: Mask = Mask.empty
):
  
  type State = ReactiveState[T, R, V]
  
  def updateValue(f: V[T] => V[T]): State =
    copy(value = f(value))
  
  def updateParentValues(f: History => History): State =
    copy(history = f(history))
  
  def updateChild(child: Contact[_, _])(f: Mask => Mask): State =
    copy(children = children + (child ->
      f(children.getOrElse(child, Mask.empty))
    ))
    
  def updateChildren(f: Mask => Mask): State =
    copy(children = children.map((c, m) => (c, f(m))))
    
  def updateParent(parent: Contact[_, _])(f: Mask => Mask): State =
    copy(parents = parents + (parent ->
      f(parents.getOrElse(parent, Mask.empty))
    ))
  
  def updateParents(f: Mask => Mask): State =
    copy(parents = parents.map((p, m) => (p, f(m))))
    
  def updatePullMask(f: Mask => Mask): State =
    copy(pullMask = f(pullMask))

object ReactiveState:
  
  type SignalState[T] = ReactiveState[T, Signal, SignalValue]
  type EventState[T] = ReactiveState[T, Event, EventValue]
  type Children = Map[Contact[_, _], Mask]
  
  case class Contact[T, R[_]](reactive: R[T], actor: ActorRef[Message])