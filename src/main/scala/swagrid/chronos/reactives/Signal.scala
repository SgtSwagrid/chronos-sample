package swagrid.chronos.reactives

import cats.*
import cats.implicits.*

import swagrid.chronos.structures.*
import swagrid.chronos.propagation.*

sealed trait Signal[+A] extends Reactive:
  
  def sample(freq: Int): Signal[A] = Signal.Sampled(this, freq)
  def changed: Event[A] = Event.Changed(this)
  
  def update(history: History): SignalValue[_ <: A]
  def parents(history: History): Map[Reactive, TemporalMask]

object Signal:
  
  def constant[A](x: A): Signal[A] = Constant(x)
  
  case class Joined[A, B](ff: Signal[A => B], fa: Signal[A]) extends Signal[B]:
    
    def update(history: History): SignalValue[B] = for
      f <- history.signalValue(ff)
      a <- history.signalValue(fa)
    yield f(a)
    
    def parents(history: History): Map[Reactive, TemporalMask] =
      Map(ff -> TemporalMask.all, fa -> TemporalMask.all)
    
  case class Switched[A, B](fa: Signal[A], f: A => Signal[B]) extends Signal[B]:
    
    def update(history: History): SignalValue[B] = for
      a <- history.signalValue(fa)
      fa <- history.signalValue(f(a))
    yield fa
  
    def parents(history: History): Map[Reactive, TemporalMask] =
      history.signalValue(fa).separate
        .asInstanceOf[Map[Reactive, TemporalMask]]
        + (fa -> TemporalMask.all)
    
  case class Held[A](parent: Event[A], initial: A) extends Signal[A]:
    
    def update(history: History): SignalValue[A] =
      history.eventValue(parent).hold
  
    def parents(history: History): Map[Reactive, TemporalMask] =
      Map(parent -> TemporalMask.all)
  
  case class Sampled[A](parent: Signal[A], freq: Int) extends Signal[A]:
    
    def update(history: History): SignalValue[A] =
      history.signalValue(parent).quantise(freq)
  
    def parents(history: History): Map[Reactive, TemporalMask] =
      Map(parent -> TemporalMask.all)
    
  case class Constant[A](x: A) extends Signal[A]:
    
    def update(history: History): SignalValue[A] =
      SignalValue.constant(x)
  
    def parents(history: History): Map[Reactive, TemporalMask] =
      Map()
  
  given Monad[Signal] with
    def pure[A](x: A): Signal[A] = constant(x)
    def flatMap[A, B](fa: Signal[A])(f: A => Signal[B]): Signal[B] = Switched(fa, f)
    def tailRecM[A, B](a: A)(f: A => Signal[Either[A, B]]): Signal[B] = { ??? }