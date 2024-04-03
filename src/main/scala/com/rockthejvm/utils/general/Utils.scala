package com.rockthejvm.utils.general

import cats.{Applicative, Functor}
import cats.effect.kernel.MonadCancel
import cats.syntax.functor.*

import scala.concurrent.duration.FiniteDuration

extension [F[_], A](fa: F[A]) {
  def debug(using functor: Functor[F]): F[A] = fa.map { a =>
    val t = Thread.currentThread().getName
    println(s"[$t] $a")
    a
  }
}

extension [F[_]](ap: Applicative[F]) {
  // Use this instead of IO.sleep
  // Not advisable - (blocks fiber) but introduced as a generalised sleep method for the sake of
  // the PolymorphicCancellation exercise
  // Cats has a Temporal typeclass we'll meet later on to handle this in the correct way
  def unsafeSleep(duration: FiniteDuration) =
    ap.pure(Thread.sleep(duration.toMillis)) // this is really blocking (not semantic blocking) so CE cannot interrupt it
}