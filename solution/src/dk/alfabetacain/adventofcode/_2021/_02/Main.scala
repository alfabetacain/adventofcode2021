package dk.alfabetacain.adventofcode._2021._02

import cats.effect.IOApp
import cats.effect.{ ExitCode, IO }
import cats.implicits._
import cats.syntax._
import cats.data.State
import monocle.syntax.all._
import dk.alfabetacain.adventofcode._2021.Util

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      _ <- solveFirstTest()
      _ <- solveFirst()
      _ <- solveSecond()
    } yield ExitCode.Success
  }

  final case class SubmarineState(depth: Long = 0, horizontal: Long = 0, aim: Long = 0) {
    def multiplied: Long = depth * horizontal
  }

  type SubState[A] = State[SubmarineState, A]

  def solveFirstTest(): IO[Unit] = {
    IO {
      val input    = Simple.parse("02/1_test.txt")
      val (res, _) = input.run(SubmarineState()).value
      println(s"First test = ${res.multiplied}")
    }
  }

  def solveFirst(): IO[Unit] = {
    IO {
      val input    = Simple.parse("02/1.txt")
      val (res, _) = input.run(SubmarineState()).value
      println(s"First = ${res.multiplied}")
    }
  }

  def solveSecond(): IO[Unit] = {
    IO {
      val input    = Advanced.parse("02/1.txt")
      val (res, _) = input.run(SubmarineState()).value
      println(s"Second = ${res.multiplied}")
    }
  }

  private def parseCommand(
      forward: Long => SubState[Unit],
      up: Long => SubState[Unit],
      down: Long => SubState[Unit]
  )(input: String): SubState[Unit] = {
    val parts   = input.split(" ")
    val command = parts(0).trim
    val steps   = parts(1).trim.toInt
    command match {
      case "forward" =>
        forward(steps)
      case "up" =>
        up(steps)
      case "down" =>
        down(steps)
      case other =>
        println(s"got other command: '$other'")
        ???
    }
  }

  object Simple {

    private def forward(steps: Long): SubState[Unit] = {
      State.modify(_.focus(_.horizontal).modify(_ + steps))
    }

    private def down(steps: Long): SubState[Unit] = {
      State.modify(_.focus(_.depth).modify(_ + steps))
    }

    private def up(steps: Long): SubState[Unit] = {
      State.modify(_.focus(_.depth).modify(_ - steps))
    }

    def parse(path: String): SubState[Unit] = {
      val parser: String => SubState[Unit] = parseCommand(
        forward = forward,
        up = up,
        down = down
      )
      Util
        .loadResource(path)
        .map(parser)
        .sequence
        .map(_ => ())
    }
  }

  object Advanced {

    private def down(steps: Long): SubState[Unit] = {
      State.modify(_.focus(_.aim).modify(_ + steps))
    }

    private def up(steps: Long): SubState[Unit] = {
      State.modify(_.focus(_.aim).modify(_ - steps))
    }

    private def forward(steps: Long): SubState[Unit] = {
      State.modify { state =>
        state
          .focus(_.horizontal).modify(_ + steps)
          .focus(_.depth).modify(_ + state.aim * steps)
      }
    }

    def parse(path: String): SubState[Unit] = {
      val parser: String => SubState[Unit] = parseCommand(
        forward = forward,
        up = up,
        down = down
      )
      Util.loadResource(path)
        .map(parser)
        .sequence
        .map(_ => ())
    }
  }
}
