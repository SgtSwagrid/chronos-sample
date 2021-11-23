package swagrid.chronos.structures

import swagrid.chronos.structures.Timestamp.*

sealed trait Timestamp extends Ordered[Timestamp]:
  
  def compare(that: Timestamp): Int = ((this, that) match
    
    case (Genesis, Genesis) => 0
    case (Genesis, _) => -1
    case (_, Genesis) => 1
    case (Infinity, Infinity) => 0
    case (Infinity, _) => 1
    case (_, Infinity) => -1
    
    case (Inclusive(x1), Inclusive(x2)) => x1 - x2
    case (Inclusive(x1), RightOf(x2)) => if (x1 == x2) -1 else (x1 - x2)
    case (Inclusive(x1), LeftOf(x2)) => if (x1 == x2) 1 else (x1 - x2)
    case (RightOf(x1), Inclusive(x2)) => if (x1 == x2) 1 else (x1 - x2)
    case (RightOf(x1), RightOf(x2)) => x1 - x2
    case (RightOf(x1), LeftOf(x2)) => if (x1 == x2) 1 else (x1 - x2)
    case (LeftOf(x1), Inclusive(x2)) => if (x1 == x2) -1 else (x1 - x2)
    case (LeftOf(x1), RightOf(x2)) => if (x1 == x2) -1 else (x1 - x2)
    case (LeftOf(x1), LeftOf(x2)) => x1 - x2
    
  ).toInt
  
  def adjacent(that: Timestamp): Boolean = (this, that) match
    
    case (Genesis, Genesis) => true
    case (Infinity, Infinity) => true
    
    case (Inclusive(x1), Inclusive(x2)) => x1 == x2
    case (RightOf(x1), RightOf(x2)) => x1 == x2
    case (LeftOf(x1), LeftOf(x2)) => x1 == x2
    case (Inclusive(x1), RightOf(x2)) => x1 == x2
    case (RightOf(x1), Inclusive(x2)) => x1 == x2
    case (Inclusive(x1), LeftOf(x2)) => x1 == x2
    case (LeftOf(x1), Inclusive(x2)) => x1 == x2

    case _ => false
    
  def roundDown(mult: Int): Timestamp = this match
    case Inclusive(x) => Inclusive(x-x%mult)
    case RightOf(x) => Inclusive(x-x%mult)
    case LeftOf(x) => Inclusive(if x%mult==0 then x-mult else x)
    case inf => inf
  
  def roundUp(mult: Int): Timestamp = this match
    case Inclusive(x) => Inclusive(if x%mult==0 then x else x+mult-x%mult)
    case RightOf(x) => Inclusive(x+mult-x%mult)
    case LeftOf(x) => Inclusive(if x%mult==0 then x else x+mult-x%mult)
    case inf => inf
  
  def excludeLeft: Timestamp = this match
    case Inclusive(x) => RightOf(x)
    case LeftOf(x) => RightOf(x)
    case pt => pt
  
  def excludeRight: Timestamp = this match
    case Inclusive(x) => LeftOf(x)
    case RightOf(x) => LeftOf(x)
    case pt => pt
    
  def includeLeft: Timestamp = excludeRight
  def includeRight: Timestamp = excludeLeft
    
  def leftAdjacent: Timestamp = this match
    case Inclusive(x) => LeftOf(x)
    case RightOf(x) => Inclusive(x)
    case pt => pt
  
  def rightAdjacent: Timestamp = this match
    case Inclusive(x) => RightOf(x)
    case LeftOf(x) => Inclusive(x)
    case pt => pt
    
  def min(that: Timestamp): Timestamp =
    if this <= that then this else that
    
  def max(that: Timestamp): Timestamp =
    if this >= that then this else that
    
  def ~> (that: Timestamp): Period =
    Period(this, that)

object Timestamp:
  
  case class Inclusive(x: Long) extends Timestamp:
    override def toString = x.toString
    
  case class RightOf(x: Long) extends Timestamp:
    override def toString = s"$x+"
    
  case class LeftOf(x: Long) extends Timestamp:
    override def toString = s"$x-"
    
  case object Genesis extends Timestamp:
    override def toString = "-\u221e"
    
  case object Infinity extends Timestamp:
    override def toString = "\u221e"