# chronos-sample

**NOTICE: This project is not yet ready for use and is provided only for grading purposes.**

Included is a subset of the code from Chronos to hopefully demonstrate that the concept is sound. It is not yet complete.

What _is_ included:
* The set of basic primitives (events, signals) and operators (map, zip, combine, etc) from sections 4.2.1 and 4.2.2 in _swagrid.chronos.reactives_.
* Time dependent values (TDVs) and temporal masks from section 4.3.2 in _swagrid.chronos.structures_. The former are called {Signal|Event}Value.
* Reactive state propagation using actors from sections 4.3.3-4.3.7 in _swagrid.chronos.propagation_. Although, this part could use a little more work.
* Custom sampling frequencies from section 4.3.8.

What _is not_ included:
* 'Real' multi-tier functionality (sections 4.2.3, 4.3.9-4.3.10, 4.3.12). For testing purposes, this can be simulated. Unlike with other FRP models, there is no global state information which prevents 'easy' generalisation to the distributed case (my implementation is a faulty mess right now, though).
* Deletion of old values (section 4.3.11) / reactives (section 4.3.13). The former is easy, see _structures.SignalValue.crop(...)_.

I'm going to continue working on (a) bringing items from the second list into the first and (b) making everying a bit more legible. Feel free to incorporate/not incorporate these changes at the time of marking as you see fit.

In the meantime, I would suggest looking at _swagrid.chronos.structures_ first. This is (I think) the most interesting part since it contains most of the stuff which is actually new about Chronos: the data structures necessary for combining higher-order/distributed reactives with complete glitch freedom.

Necessary imports:
_cats.implicits.*_
_akka.actor.typed.{ActorRef, ActorSystem, Behavior}_
