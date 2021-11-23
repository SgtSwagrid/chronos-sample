package swagrid.chronos.reactives

import cats.*
import cats.implicits.*

import swagrid.chronos.structures.*
import swagrid.chronos.propagation.*

sealed trait Event[+A] extends Reactive:
  
  def filter(f: A => Boolean): Event[A] = Event.Filtered(this, f)
  def scan[B](initial: B)(f: (B, A) => B): Event[B] = Event.Scanned(this, initial, f)
  def hold[B](initial: B): Signal[A | B] = Signal.Held(this, initial)
  def fold[B](initial: B)(f: (B, A) => B): Signal[B] = scan(initial)(f).hold(initial)
  def emit[B](source: Signal[B]): Event[B] = Event.Emitted(this, source)
  
  def update(history: History): EventValue[_ <: A]
  def parents(history: History): Map[Reactive, Mask]

object Event:
  
  def genesis[A](e: A): Event[A] = Genesis(e)
  
  def empty: Event[Nothing] = Empty
  
  case class Source[A]() extends Event[A]:
  
    def update(history: History): EventValue[A] =
      ???
  
    def parents(history: History): Map[Reactive, Mask] =
      ???
    
  case class Joined[A, B](ff: Event[A => B], fa: Event[A]) extends Event[B]:
    
    def update(history: History): EventValue[B] = for
      f <- history.eventValue(ff)
      a <- history.eventValue(fa)
    yield f(a)
  
    def parents(history: History): Map[Reactive, Mask] =
      Map(ff -> Mask.all, fa -> Mask.all)
    
  case class Switched[A, B](fa: Event[A], f: A => Event[B]) extends Event[B]:
  
    def update(history: History): EventValue[B] = for
      a <- history.eventValue(fa)
      fa <- history.eventValue(f(a))
    yield fa
  
    def parents(history: History): Map[Reactive, Mask] =
      history.eventValue(fa).intervals.separate
        .asInstanceOf[Map[Reactive, Mask]]
        + (fa -> Mask.all)
    
  case class Combined[A, B](a: Event[A], b: Event[B]) extends Event[A | B]:
  
    def update(history: History): EventValue[A | B] =
      history.eventValue(a) + history.eventValue(b)
  
    def parents(history: History): Map[Reactive, Mask] =
      Map(a -> Mask.all, b -> Mask.all)
    
  case class Filtered[A](parent: Event[A], f: A => Boolean) extends Event[A]:
  
    def update(history: History): EventValue[A] =
      history.eventValue(parent).filter(f)
  
    def parents(history: History): Map[Reactive, Mask] =
      Map(parent -> Mask.all)
    
  case class Scanned[A, B](parent: Event[A], initial: B, f: (B, A) => B) extends Event[B]:
  
    def update(history: History): EventValue[B] =
      history.eventValue(this).accumulate(history.eventValue(parent))(f)
  
    def parents(history: History): Map[Reactive, Mask] =
      Map(parent -> Mask.all)
    
  case class Emitted[A](trigger: Event[Any], source: Signal[A]) extends Event[A]:
  
    def update(history: History): EventValue[A] =
      history.eventValue(trigger).copy(history.signalValue(source))
  
    def parents(history: History): Map[Reactive, Mask] =
      Map(trigger -> Mask.all, source -> Mask.all)
    
  case class Changed[A](parent: Signal[A]) extends Event[A]:
    
    def update(history: History): EventValue[A] =
      history.signalValue(parent).changes
  
    def parents(history: History): Map[Reactive, Mask] =
      Map(parent -> Mask.all)
    
  case class Genesis[A](e: A) extends Event[A]:
  
    def update(history: History): EventValue[A] =
      EventValue.genesis(e)
  
    def parents(history: History): Map[Reactive, Mask] =
      Map()
    
  case object Empty extends Event[Nothing]:
  
    def update(history: History): EventValue[Nothing] =
      EventValue.empty
  
    def parents(history: History): Map[Reactive, Mask] =
      Map()
  
  given Monad[Event] with
    def pure[A](x: A): Event[A] = genesis(x)
    def flatMap[A, B](fa: Event[A])(f: A => Event[B]): Event[B] = Switched(fa, f)
    def tailRecM[A, B](a: A)(f: A => Event[Either[A, B]]): Event[B] = { ??? }
    
  given MonoidK[Event] with
    def empty[A]: Event[Nothing] = Event.empty
    def combineK[A](x: Event[A], y: Event[A]): Event[A] = Combined(x, y)