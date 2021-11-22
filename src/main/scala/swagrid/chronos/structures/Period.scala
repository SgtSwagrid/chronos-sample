package swagrid.chronos.structures

import scala.collection.immutable.TreeSet

case class Period(start: Endpoint, end: Endpoint) extends Ordered[Period]:
  
  def intersect(that: Period): Option[Period] =
    val left = start.max(that.start)
    val right = end.min(that.end)
    Option.when(left <= right)(Period(left, right))
    
  def span(that: Period): Period =
    val left = start.min(that.start)
    val right = end.max(that.end)
    Period(left, right)
    
  def overlapping(that: Period): Boolean =
    intersect(that).isDefined
  
  def compare(that: Period): Int =
    if overlapping(that) then 0 else start.compare(that.start)
    
  def quantise(freq: Int): Option[Period] =
    val left = start.roundUp(freq)
    val right = end.roundUp(freq).excludeRight
    Option.when(left <= right)(Period(left, right))
  
  override def toString = s"[$start:$end]"
  
object Period:
  
  def instant(t: Endpoint): Period = Period(t, t)
  def all: Period = Period(Endpoint.NegativeInfinity, Endpoint.PositiveInfinity)