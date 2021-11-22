package swagrid.chronos.structures

import swagrid.chronos.structures.Endpoint.*

sealed trait Endpoint extends Ordered[Endpoint]:
  
  def compare(that: Endpoint): Int = ((this, that) match
    
    case (NegativeInfinity, NegativeInfinity) => 0
    case (NegativeInfinity, _) => -1
    case (_, NegativeInfinity) => 1
    case (PositiveInfinity, PositiveInfinity) => 0
    case (PositiveInfinity, _) => 1
    case (_, PositiveInfinity) => -1
    
    case (Inclusive(x1), Inclusive(x2)) => x1 - x2
    case (Inclusive(x1), LeftExclusive(x2)) => if (x1 == x2) -1 else (x1 - x2)
    case (Inclusive(x1), RightExclusive(x2)) => if (x1 == x2) 1 else (x1 - x2)
    case (LeftExclusive(x1), Inclusive(x2)) => if (x1 == x2) 1 else (x1 - x2)
    case (LeftExclusive(x1), LeftExclusive(x2)) => x1 - x2
    case (LeftExclusive(x1), RightExclusive(x2)) => if (x1 == x2) 1 else (x1 - x2)
    case (RightExclusive(x1), Inclusive(x2)) => if (x1 == x2) -1 else (x1 - x2)
    case (RightExclusive(x1), LeftExclusive(x2)) => if (x1 == x2) -1 else (x1 - x2)
    case (RightExclusive(x1), RightExclusive(x2)) => x1 - x2
    
  ).toInt
  
  def adjacent(that: Endpoint): Boolean = (this, that) match
    
    case (NegativeInfinity, NegativeInfinity) => true
    case (PositiveInfinity, PositiveInfinity) => true
    
    case (Inclusive(x1), Inclusive(x2)) => x1 == x2
    case (LeftExclusive(x1), LeftExclusive(x2)) => x1 == x2
    case (RightExclusive(x1), RightExclusive(x2)) => x1 == x2
    case (Inclusive(x1), LeftExclusive(x2)) => x1 == x2
    case (LeftExclusive(x1), Inclusive(x2)) => x1 == x2
    case (Inclusive(x1), RightExclusive(x2)) => x1 == x2
    case (RightExclusive(x1), Inclusive(x2)) => x1 == x2

    case _ => false
    
  def roundDown(mult: Int): Endpoint = this match
    case Inclusive(x) => Inclusive(x-x%mult)
    case LeftExclusive(x) => Inclusive(x-x%mult)
    case RightExclusive(x) => Inclusive(if x%mult==0 then x-mult else x)
    case inf => inf
  
  def roundUp(mult: Int): Endpoint = this match
    case Inclusive(x) => Inclusive(if x%mult==0 then x else x+mult-x%mult)
    case LeftExclusive(x) => Inclusive(x+mult-x%mult)
    case RightExclusive(x) => Inclusive(if x%mult==0 then x else x+mult-x%mult)
    case inf => inf
  
  def excludeLeft: Endpoint = this match
    case Inclusive(x) => LeftExclusive(x)
    case RightExclusive(x) => throw IllegalStateException
      ("Attempting to exclude internal point from period.")
    case pt => pt
  
  def excludeRight: Endpoint = this match
    case Inclusive(x) => RightExclusive(x)
    case LeftExclusive(x) => throw IllegalStateException
      ("Attempting to exclude internal point from period.")
    case pt => pt
    
  def leftAdjacent: Endpoint = this match
    case Inclusive(x) => RightExclusive(x)
    case LeftExclusive(x) => Inclusive(x)
    case RightExclusive(x) => throw IllegalStateException
      ("Right-exclusive points have no adjacent point to the immediate left.")
    case inf => inf
  
  def rightAdjacent: Endpoint = this match
    case Inclusive(x) => LeftExclusive(x)
    case RightExclusive(x) => Inclusive(x)
    case LeftExclusive(x) => throw IllegalStateException
      ("Left-exclusive points have no adjacent point to the immediate right.")
    case inf => inf
    
  def min(that: Endpoint): Endpoint =
    if this <= that then this else that
    
  def max(that: Endpoint): Endpoint =
    if this >= that then this else that
    
  def ~> (that: Endpoint): Period =
    Period(this, that)

object Endpoint:
  
  case class Inclusive(x: Long) extends Endpoint:
    override def toString = x.toString
    
  case class LeftExclusive(x: Long) extends Endpoint:
    override def toString = s"$x+"
    
  case class RightExclusive(x: Long) extends Endpoint:
    override def toString = s"$x-"
    
  case object NegativeInfinity extends Endpoint:
    override def toString = "-\u221e"
    
  case object PositiveInfinity extends Endpoint:
    override def toString = "\u221e"