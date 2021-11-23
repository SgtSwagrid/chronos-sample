package swagrid.chronos.structures

import scala.collection.immutable.{TreeMap, TreeSet}
import cats.*
import swagrid.chronos.structures.SignalValue.*

class SignalValue[+T] private[structures]
  (val history: TreeMap[Period, T]):

  def + [U](that: SignalValue[U]): SignalValue[T | U] =
    new SignalValue(consolidate(history ++ that.history))
  
  def crop(period: Period): SignalValue[T] =
    val h = history
      .rangeFrom(Period.instant(period.start))
      .rangeTo(Period.instant(period.end))
    new SignalValue(h.flatMap((p, x) => p.intersect(period).map(_ -> x)))
    
  def crop(mask: Mask): SignalValue[T] =
    mask.periods.unsorted.map(crop).fold(SignalValue.unknown)(_ + _)
    
  def flatMap[U](f: T => SignalValue[U]): SignalValue[U] =
    val h = history.flatMap((p0, x0) => f(x0).crop(p0).history)
    new SignalValue(consolidate(h))
  
  def mask: Mask =
    Mask(history.keySet.toSeq*)
    
  def apply(t: Timestamp): Option[T] =
    crop(Period.instant(t)).history.headOption.map((_, x) => x)
    
  def quantise(freq: Int): SignalValue[T] =
    val h = history.flatMap((p, x) => p.quantise(freq).map(_ -> x))
    new SignalValue(consolidate(h))
    
  def fill[U](value: U): SignalValue[T | U] =
    
    val start = Period.instant(Timestamp.Genesis)
    val end = Period.instant(Timestamp.Infinity)
    val periods = start +: history.keySet.toSeq :+ end
    
    val gaps = periods.sliding(2).map(p => (p(0), p(1)))
      .filter((l, r) => !l.end.adjacent(r.start))
      .map((l, r) => Period(l.end.rightAdjacent, r.start.leftAdjacent))
    
    new SignalValue(history ++ gaps.map(_ -> value))
    
  def changes: EventValue[T] =
    val h = TreeMap.from(history.toSeq.sliding(2).flatMap {
      case Seq((p0, x0), (p1, x1)) =>
        Option.when(p0.end.adjacent(p1.start))(p1.start -> Seq(x1))
    })
    new EventValue(h, mask.trimLeft)
    
  def separate: Map[_ <: T, Mask] =
    history.values.toSet.map { x0 =>
      x0 -> Mask(history.filter((_, x1) => x0 == x1).keys.toSeq*)
    }.toMap
    
  def isEmpty: Boolean = history.isEmpty
    
  override def toString: String =
    history.map((p, x) => s"$x $p").mkString("{", ", ", "}")
  
object SignalValue:
  
  def apply[T](history: (Period, T)*): SignalValue[T] =
    new SignalValue(consolidate(TreeMap.from(history)))
    
  def constant[T](x: T): SignalValue[T] =
    SignalValue(Period.all -> x)
    
  def unknown: SignalValue[Nothing] =
    SignalValue()
  
  private def consolidate[T](history: TreeMap[Period, T]): TreeMap[Period, T] =
    
    history.foldRight(TreeMap[Period, T]()) { case ((p0, x0), h) =>
      
      val (same, different) = h
        .takeWhile((p1, _) => p0.overlapping(p1) || p0.end.adjacent(p1.start))
        .partition((p1, x1) => x0 == x1)
      
      if different.exists((p1, _) => p0.overlapping(p1)) then
        throw IllegalStateException("Attempting to combine incompatible value histories.")
      
      val p = same.keySet.fold(p0)(_ span _)
      h -- same.keySet + (p -> x0)
    }
  
  extension[T] (sv: SignalValue[EventValue[T]])
    def flattenEvent: EventValue[T] =
      sv.history.map((p, ev) => ev.crop(p)).fold(EventValue.unknown)(_ + _)
  
  extension[T] (sv: SignalValue[Option[T]])
    def flattenOption: SignalValue[T] = sv.flatMap {
      case Some(x) => constant(x)
      case None => unknown
    }
    
  given Monad[SignalValue] with
    def pure[A](x: A): SignalValue[A] = constant(x)
    def flatMap[A, B](fa: SignalValue[A])(f: A => SignalValue[B]): SignalValue[B] = fa.flatMap(f)
    def tailRecM[A, B](a: A)(f: A => SignalValue[Either[A, B]]): SignalValue[B] = { ??? }
    
  given MonoidK[SignalValue] with
    def empty[A]: SignalValue[Nothing] = SignalValue.unknown
    def combineK[A](x: SignalValue[A], y: SignalValue[A]): SignalValue[A] = x + y