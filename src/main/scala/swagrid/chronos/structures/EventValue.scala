package swagrid.chronos.structures

import scala.collection.immutable.TreeMap

import cats.*
import cats.implicits.*

class EventValue[+T] private[structures]
    (val stream: SignalValue[(Endpoint, T)]):
  
  def combine[U](that: EventValue[U]): EventValue[T | U] =
    val s = (stream, that.stream).mapN { case ((t0, e0), (t1, e1)) =>
      if t0 > t1 then (t0, e0) else (t1, e1)
    }
    new EventValue(s)
  
  def + [U](that: EventValue[U]): EventValue[T | U] = combine(that)

  def crop(period: Period): EventValue[T] =
    new EventValue(stream.crop(period))
  
  def crop(mask: TemporalMask): EventValue[T] =
    new EventValue(stream.crop(mask))
    
  def flatMap[U](f: T => EventValue[U]): EventValue[U] =
    new EventValue(stream.flatMap((_, x) => f(x).stream))
    
  def mask: TemporalMask = stream.mask
  
  def filter(f: T => Boolean): EventValue[T] =
    
    val h = stream.history.foldLeft(TreeMap[Period, (Endpoint, T)]()) {
      case (h, (p1, (t1, e1))) =>
        
        if f(e1) then h + (p1 -> (t1, e1))
        else h.lastOption match
          case Some((p0, e0)) if p0.end.adjacent(p1.start) =>
            h - p0 + (p0.span(p1) -> e0)
          case _ => h
    }
    
    new EventValue(new SignalValue(h))
    
  def hold: SignalValue[T] =
    stream.map((_, x) => x)
    
  def emit[U](signal: SignalValue[U]): EventValue[U] =
    val updated = stream.history.flatMap {
      case (p, (t, x)) => signal(t).map(s => p -> (t, s))
    }
    new EventValue(new SignalValue(updated))
    
  def scan[U](initial: U)(f: (U, T) => U): EventValue[U] =
    val values = stream.history.values.map((_, x) => x)
      .scanLeft(initial)(f)
    val scanned = stream.history.zip(values)
      .map { case ((p, (t, x0)), x1) => p -> (t, x1) }
    new EventValue(new SignalValue(TreeMap.from(scanned)))
    
  def separate: Map[_ <: T, TemporalMask] =
    stream.separate.map { case ((t, x), m) => x -> m }
    
  def isEmpty: Boolean = stream.isEmpty
    
  override def toString: String = stream.history
    .map { case (p, (t, e)) => s"$e:$t $p" }
    .mkString("{", ", ", "}")
  
object EventValue:
  
  def apply[T](history: (Period, T)*): EventValue[T] =
    val h = history.map((p, e) => p -> (p.start, e))
    new EventValue(SignalValue(h*))
    
  def genesis[T](e: T): EventValue[T] =
    EventValue(Period.all -> e)
  
  def empty: EventValue[Nothing] =
    EventValue()
    
  given Monad[EventValue] with
    def pure[A](x: A): EventValue[A] = genesis(x)
    def flatMap[A, B](fa: EventValue[A])(f: A => EventValue[B]): EventValue[B] = fa.flatMap(f)
    def tailRecM[A, B](a: A)(f: A => EventValue[Either[A, B]]): EventValue[B] = { ??? }
  
  given MonoidK[EventValue] with
    def empty[A]: EventValue[Nothing] = EventValue.empty
    def combineK[A](x: EventValue[A], y: EventValue[A]): EventValue[A] = x.combine(y)