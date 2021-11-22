package swagrid.chronos.structures

import scala.collection.immutable.TreeSet
import cats.implicits.*

class TemporalMask private (val mask: SignalValue[Boolean]):
  
  def & (that: TemporalMask): TemporalMask =
    new TemporalMask((mask, that.mask).mapN(_ && _))
  
  def | (that: TemporalMask): TemporalMask =
    new TemporalMask((mask, that.mask).mapN(_ || _))
  
  def ^ (that: TemporalMask): TemporalMask =
    new TemporalMask((mask, that.mask).mapN(_ ^ _))
    
  def - (that: TemporalMask): TemporalMask =
    this & !that
    
  def unary_! : TemporalMask =
    new TemporalMask(mask.map(!_))
    
  def periods: TreeSet[Period] =
    mask.history.filter((_, x) => x).keySet
    
  def isEmpty: Boolean = periods.isEmpty
    
  override def toString: String = periods.mkString("{", ", ", "}")
  
  override def equals(that: Any): Boolean = that match
    case that: TemporalMask => periods == that.periods
    case _ => false
  
object TemporalMask:
  
  def apply(periods: Period*): TemporalMask =
    val signal = SignalValue(periods.map(_ -> true)*)
    new TemporalMask(signal.fill(false))
    
  def empty: TemporalMask = TemporalMask()
  def all: TemporalMask = TemporalMask(Endpoint.NegativeInfinity ~> Endpoint.PositiveInfinity)