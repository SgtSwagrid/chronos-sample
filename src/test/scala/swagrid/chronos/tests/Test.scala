package swagrid.chronos.tests

import cats.implicits.*
import swagrid.chronos.reactives.Event

@main def test =
  
  val e = Event.genesis(3)