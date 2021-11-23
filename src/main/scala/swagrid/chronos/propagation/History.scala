package swagrid.chronos.propagation

import swagrid.chronos.reactives.*
import swagrid.chronos.structures.*

case class History (
  signals: Map[Signal[Any], SignalValue[Any]] = Map(),
  events: Map[Event[Any], EventValue[Any]] = Map()
):
  
  def signalValue[T](signal: Signal[T]): SignalValue[T] =
    signals.get(signal).map(_.asInstanceOf[SignalValue[T]]).getOrElse(SignalValue.unknown)
  
  def eventValue[T](event: Event[T]): EventValue[T] =
    events.get(event).map(_.asInstanceOf[EventValue[T]]).getOrElse(EventValue.empty)
  
  def updateSignal[T, R[_], V[_]](signal: Signal[T], update: SignalValue[T]): History =
    val updated = (Some(update) ++ signals.get(signal)).reduce(_ + _)
    copy(signals = signals + (signal -> updated))
  
  def updateEvent[T](event: Event[T], update: EventValue[T]): History =
    val updated = (Some(update) ++ events.get(event)).reduce(_ + _)
    copy(events = events + (event -> updated))
  
  def crop(mask: Mask): History = copy (
    signals = signals.map((s, sv) => (s, sv.crop(mask))),
    events = events.map((e, ev) => (e, ev.crop(mask)))
  )
  
  def + (that: History): History =
    
    val s = (signals.keySet | that.signals.keySet)
      .map(s => s -> (signals.get(s) ++ that.signals.get(s)).reduce(_ + _)).toMap
  
    val e = (events.keySet | that.events.keySet)
      .map(e => e -> (events.get(e) ++ that.events.get(e)).reduce(_ + _)).toMap
    
    copy(signals = s, events = e)