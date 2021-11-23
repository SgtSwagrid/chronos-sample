package swagrid.chronos.structures

import scala.collection.immutable.TreeSet

case class Period(start: Timestamp, end: Timestamp) extends Ordered[Period]:
  
  def intersect(that: Period): Option[Period] =
    val left = start.max(that.start)
    val right = end.min(that.end)
    Option.when(left <= right)(Period(left, right))
    
  def span(that: Period): Period =
    val left = start.min(that.start)
    val right = end.max(that.end)
    Period(left, right)
    
  def trimLeft: Period = Period(start.rightAdjacent, end)
  def trimRight: Period = Period(start, end.leftAdjacent)
  def trim: Period = Period(start.rightAdjacent, end.leftAdjacent)
  
  def expandLeft: Period = Period(start.leftAdjacent, end)
  def expandRight: Period = Period(start, end.rightAdjacent)
  def expand: Period = Period(start.leftAdjacent, end.rightAdjacent)
    
  def overlapping(that: Period): Boolean =
    intersect(that).isDefined
    
  def contains(t: Timestamp): Boolean =
    start <= t && t <= end
  
  def compare(that: Period): Int =
    if overlapping(that) then 0 else start.compare(that.start)
    
  def quantise(freq: Int): Option[Period] =
    val left = start.roundUp(freq)
    val right = end.roundUp(freq).excludeRight
    Option.when(left <= right)(Period(left, right))
    
  def mask: Mask = Mask(this)
  
  override def toString = s"[$start:$end]"
  
object Period:
  
  def instant(t: Timestamp): Period = Period(t, t)
  def all: Period = Period(Timestamp.Genesis, Timestamp.Infinity)