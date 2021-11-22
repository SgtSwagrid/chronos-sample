package swagrid.chronos.structures

import scala.collection.immutable.{TreeMap, TreeSet}

import cats.*

import swagrid.chronos.structures.SignalValue.*

class SignalValue[+T] private[structures]
  (val history: TreeMap[Period, T]):

  def combine[U](that: SignalValue[U]): SignalValue[T | U] =
    new SignalValue(consolidate(history ++ that.history))
  
  def + [U](that: SignalValue[U]): SignalValue[T | U] = combine(that)
  
  def crop(period: Period): SignalValue[T] =
    val h = history
      .rangeFrom(Period.instant(period.start))
      .rangeTo(Period.instant(period.end))
    new SignalValue(h.flatMap((p, x) => p.intersect(period).map(_ -> x)))
    
  def crop(mask: TemporalMask): SignalValue[T] =
    mask.periods.unsorted.map(crop).reduce(_ combine _)
    
  def flatMap[U](f: T => SignalValue[U]): SignalValue[U] =
    val h = history.flatMap((p0, x0) => f(x0).crop(p0).history)
    new SignalValue(consolidate(h))
  
  def mask: TemporalMask =
    TemporalMask(history.keySet.toSeq*)
    
  def apply(t: Endpoint): Option[T] = history
    .rangeFrom(Period.instant(t))
    .rangeTo(Period.instant(t))
    .values.headOption
    
  def quantise(freq: Int): SignalValue[T] =
    val h = history.flatMap((p, x) => p.quantise(freq).map(_ -> x))
    new SignalValue(consolidate(h))
    
  def fill[U](value: U): SignalValue[T | U] =
    
    val start = Period.instant(Endpoint.NegativeInfinity)
    val end = Period.instant(Endpoint.PositiveInfinity)
    val periods = start +: history.keySet.toSeq :+ end
    
    val gaps = periods.sliding(2).map(p => (p(0), p(1)))
      .filter((l, r) => !l.end.adjacent(r.start))
      .map((l, r) => Period(l.end.rightAdjacent, r.start.leftAdjacent))
    
    new SignalValue(history ++ gaps.map(_ -> value))
    
  def changes: EventValue[T] =
    new EventValue(new SignalValue(history.map((p, x) => p -> (p.start, x))))
    
  def separate: Map[_ <: T, TemporalMask] =
    history.values.toSet.map { x0 =>
      x0 -> TemporalMask(history.filter((_, x1) => x0 == x1).keys.toSeq*)
    }.toMap
    
  def isEmpty: Boolean = history.isEmpty
    
  override def toString: String =
    history.map((p, x) => s"$x $p").mkString("{", ", ", "}")
  
object SignalValue:
  
  def apply[T](history: (Period, T)*): SignalValue[T] =
    new SignalValue(consolidate(TreeMap.from(history)))
    
  def constant[T](x: T): SignalValue[T] =
    SignalValue(Period.all -> x)
    
  def empty: SignalValue[Nothing] =
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
    
  extension[T] (sv: SignalValue[Option[T]])
    def flattenOption: SignalValue[T] = sv.flatMap {
      case Some(x) => constant(x)
      case None => empty
    }
    
  given Monad[SignalValue] with
    def pure[A](x: A): SignalValue[A] = constant(x)
    def flatMap[A, B](fa: SignalValue[A])(f: A => SignalValue[B]): SignalValue[B] = fa.flatMap(f)
    def tailRecM[A, B](a: A)(f: A => SignalValue[Either[A, B]]): SignalValue[B] = { ??? }
    
  given MonoidK[SignalValue] with
    def empty[A]: SignalValue[Nothing] = SignalValue.empty
    def combineK[A](x: SignalValue[A], y: SignalValue[A]): SignalValue[A] = x.combine(y)