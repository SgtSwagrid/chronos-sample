package swagrid.chronos.reactives

import cats.*

class System private (val sinks: Map[Sink[_], Event[_]])

object System:
  
  def apply[A](sink: Sink[_ >: A], event: Event[_ <: A]): System =
    new System(Map(sink -> event))
    
  def empty: System = new System(Map())
  
  given Monoid[System] with
    def empty: System = System.empty
    def combine(x: System, y: System): System = new System(x.sinks ++ y.sinks)