package swagrid.chronos.reactives

import cats.*

class Sink[-A](val effect: A => Unit)

object Sink:
  
  given Contravariant[Sink] with
    def contramap[A, B](fa: Sink[A])(f: B => A): Sink[B] =
      Sink(f.andThen(fa.effect))