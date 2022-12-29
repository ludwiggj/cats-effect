package com.rockthejvm.part2effects

import cats.effect.{ExitCode, IO, IOApp}
import com.rockthejvm.part2effects.L05_IOApps.program

import scala.io.StdIn

object L05_IOApps{
  val program = for {
    line <- IO(StdIn.readLine())
    _ <- IO.println(s"You Entered: $line")
  } yield ()

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global

    program.unsafeRunSync()
  }
}

object FirstCEApp extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    program.as(ExitCode.Success)
}

object SimpleCEApp extends IOApp.Simple {
  override def run: IO[Unit] = program
}
