package swagrid.chronos.tests

import cats.implicits.*

import swagrid.chronos.structures.*
import swagrid.chronos.structures.Timestamp.*

@main def test =
  
  val e1 = EventValue (
    Inclusive(0) -> "Hello",
    Inclusive(3) -> "World"
  )(Mask(Inclusive(0) ~> LeftOf(4)))
  
  val e2 = EventValue (
    Inclusive(2) -> "Bonjour",
    Inclusive(5) -> "Alec",
  )(Mask(Inclusive(2) ~> LeftOf(6)))
  
  println(e1 + e2)
  println((e1 + e2).map(_.toUpperCase))
  println((e1 + e2).filter(_.length <= 5))
  println((e1 + e2).intervals)
  println(e1 merge e2)