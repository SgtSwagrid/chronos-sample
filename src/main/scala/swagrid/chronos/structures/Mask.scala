package swagrid.chronos.structures

import scala.collection.immutable.TreeSet
import cats.implicits.*

class Mask private (val mask: SignalValue[Boolean]):
  
  def & (that: Mask): Mask =
    new Mask((mask, that.mask).mapN(_ && _))
  
  def | (that: Mask): Mask =
    new Mask((mask, that.mask).mapN(_ || _))
  
  def ^ (that: Mask): Mask =
    new Mask((mask, that.mask).mapN(_ ^ _))
    
  def - (that: Mask): Mask =
    this & !that
    
  def unary_! : Mask =
    new Mask(mask.map(!_))
    
  def periods: TreeSet[Period] =
    mask.history.filter((_, x) => x).keySet
  
  def trimLeft: Mask = Mask(periods.toSeq.map(_.trimLeft)*)
  def trimRight: Mask = Mask(periods.toSeq.map(_.trimRight)*)
  def trim: Mask = Mask(periods.toSeq.map(_.trim)*)
  
  def expandLeft: Mask = Mask(periods.toSeq.map(_.expandLeft)*)
  def expandRight: Mask = Mask(periods.toSeq.map(_.expandRight)*)
  def expand: Mask = Mask(periods.toSeq.map(_.expand)*)
    
  def contains(t: Timestamp): Boolean =
    periods.exists(_.contains(t))
    
  def isEmpty: Boolean = periods.isEmpty
    
  override def toString: String = periods.mkString("{", ", ", "}")
  
  override def equals(that: Any): Boolean = that match
    case that: Mask => periods == that.periods
    case _ => false
  
object Mask:
  
  def apply(periods: Period*): Mask =
    val signal = SignalValue(periods.map(_ -> true)*)
    new Mask(signal.fill(false))
    
  def empty: Mask = Mask()
  def all: Mask = Mask(Timestamp.Genesis ~> Timestamp.Infinity)