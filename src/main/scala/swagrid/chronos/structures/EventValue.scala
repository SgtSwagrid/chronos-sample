package swagrid.chronos.structures

import scala.collection.immutable.{TreeMap, TreeSet}

import cats.*
import cats.implicits.*

class EventValue[+T] private[structures]
    (val history: TreeMap[Timestamp, Seq[T]], val mask: Mask):
  
  def + [U](that: EventValue[U]): EventValue[T|U] =
    
    val compatible = (history.keySet & that.history.keySet)
      .forall(t => history(t) == that.history(t))
    if !compatible then
      throw IllegalStateException("Attempting to combine incompatible value histories.")
    
    new EventValue(history ++ that.history, mask | that.mask)
    
  def merge[U](that: EventValue[U]): EventValue[T|U] =
    
    val tn = (history.keySet | that.history.keySet).unsorted
    val merged = tn.map { t => t ->
      (history.get(t) ++ that.history.get(t)).flatten.toSeq
        .asInstanceOf[Seq[T|U]]
    }
  
    new EventValue(TreeMap.from(merged), mask & that.mask)
      .crop(mask & that.mask)

  def crop(period: Period): EventValue[T] =
    val h = history.rangeFrom(period.start).rangeTo(period.end)
    new EventValue(h, mask & period.mask)
  
  def crop(mask: Mask): EventValue[T] =
    mask.periods.unsorted.map(crop).fold(EventValue.unknown)(_ + _)
    
  def map[U](f: T => U): EventValue[U] =
    val h = history.map((t, e) => t -> e.map(f))
    new EventValue(h, mask)
  
  def flatMap[U](f: T => EventValue[U]): EventValue[U] =
    intervals.map(f).flattenEvent
  
  def filter(f: T => Boolean): EventValue[T] =
    val h = history.map((t, e) => t -> e.filter(f))
      .filter((_, e) => !e.isEmpty)
    new EventValue(h, mask)
    
  def intervals: SignalValue[T] =
    
    val periods = (history.keySet + Timestamp.Infinity)
      .toSeq.sliding(2).map { case Seq(t0, t1) => t0 ->
        ((t0 ~> t1.excludeRight).mask & mask)
          .periods.find(_.contains(t0))
      }.toMap
    
    val signals = for
      (t, e) <- history.map((t, e) => (t, e.last))
      p <- periods(t)
    yield SignalValue(p -> e)
    
    signals.fold(SignalValue.unknown)(_ + _)
    
  def copy[U](signal: SignalValue[U]): EventValue[U] =
    val h = history.flatMap { (t, e) =>
      signal(t).map(s => t -> Seq.fill(e.size)(s))
    }
    new EventValue(h, mask & signal.mask)
    
  def accumulate[U, V](source: EventValue[V])(f: (T|U, V) => U): EventValue[T|U] =
    
    val parts = for
      p0 <- mask.periods.filter(p => !crop(p).isEmpty).toSeq
      p1 <- source.crop(!mask).mask.periods.find(p1 => p0.end.adjacent(p1.start))
    yield
      val e0 = crop(p0).history.values.last.last
      val values: Iterable[T|U] =
        source.crop(p1).history.values.flatten.scanLeft(e0)(f).tail
      val times = source.crop(p1).history.flatMap((t, e) => Seq.fill(e.size)(t))
      EventValue((times zip values).toSeq*)(p1.mask)
    
    this + parts.fold(EventValue.unknown)(_ + _)
    
  def isEmpty: Boolean = history.isEmpty
    
  override def toString: String =
    mask.periods.map { p => p ->
      crop(p.mask).history.toSeq.flatMap((t, e) => e.map(t -> _))
        .map((t, e) => s"$e:$t").mkString("{", ", ", "}")
    }.map((p, x) => s"$x $p").mkString("{", ", ", "}")
  
object EventValue:
  
  def apply[T](history: (Timestamp, T)*)(mask: Mask): EventValue[T] =
    val h = history.groupBy((t, _) => t)
      .map((t, e) => t -> e.map((_, e) => e))
    new EventValue(TreeMap.from(h), mask).crop(mask)
    
  def genesis[T](e: T): EventValue[T] =
    EventValue(Timestamp.Genesis -> e)(Mask.all)
  
  def empty: EventValue[Nothing] =
    new EventValue(TreeMap(), Mask.all)
  
  def unknown: EventValue[Nothing] =
    new EventValue(TreeMap(), Mask.empty)
    
  given FlatMap[EventValue] with
    def map[A, B](fa: EventValue[A])(f: A => B): EventValue[B] = fa.map(f)
    def flatMap[A, B](fa: EventValue[A])(f: A => EventValue[B]): EventValue[B] = fa.flatMap(f)
    def tailRecM[A, B](a: A)(f: A => EventValue[Either[A, B]]): EventValue[B] = { ??? }
  
  given MonoidK[EventValue] with
    def empty[A]: EventValue[Nothing] = EventValue.unknown
    def combineK[A](x: EventValue[A], y: EventValue[A]): EventValue[A] = x + y