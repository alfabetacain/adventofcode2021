package dk.alfabetacain.adventofcode._2021._11

import cats.syntax._
import cats.implicits._
import cats.effect.IOApp
import cats.effect.{ ExitCode, IO }
import dk.alfabetacain.adventofcode._2021.Util
import cats.data.State
import scala.annotation.tailrec
import cats.kernel.Monoid

object Main extends IOApp {

  type Point = (Int, Int)

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      _ <- solveFirstTest()
      _ <- solveFirst()
      _ <- solveSecondTest()
      _ <- solveSecond()
    } yield ExitCode.Success
  }

  private def solveFirstTest(): IO[Unit] = {
    IO {
      val transformation = Stream.from(0).take(100).map(_ => OctoState.step).toList.sequence
      val result         = solve("11/1_test.txt", transformation)._2.sum
      println(s"First test = ${result}")
    }
  }

  private def solveFirst(): IO[Unit] = {
    IO {
      val transformation = Stream.from(0).take(100).map(_ => OctoState.step).toList.sequence
      val result         = solve("11/1.txt", transformation)._2.sum
      println(s"First = ${result}")
    }
  }

  private def solveSecondTest(): IO[Unit] = {
    IO {
      val result = solve("11/1_test.txt", OctoState.untilFullFlash)._2
      println(s"Second test = ${result}")
    }
  }

  private def solveSecond(): IO[Unit] = {
    IO {
      val result = solve("11/1.txt", OctoState.untilFullFlash)._2
      println(s"Second = ${result}")
    }
  }

  private def solve[A](path: String, transformation: OctoState[A]): (Map[Point, Int], A) = {
    val input = loadInput(path)
    transformation.run(input).value
  }

  type OctoState[A] = State[Map[Point, Int], A]

  object OctoState {

    private def adjacents(point: Point): List[Point] = {
      List(
        (-1, -1),
        (0, -1),
        (1, -1),
        (-1, 0),
        (0, 0),
        (1, 0),
        (-1, 1),
        (0, 1),
        (1, 1)
      ).map(Monoid[Point].combine(point, _))
    }

    private def flash(octopi: Map[Point, Int], octopus: Point): (List[Point], Map[Point, Int]) = {
      val adjacentOctopi = adjacents(octopus)
      adjacentOctopi.foldLeft[(List[Point], Map[Point, Int])]((List.empty, octopi + (octopus -> 0))) {
        case ((newFlashes, acc), current) =>
          acc.get(current) match {
            case Some(value) if value != 0 =>
              val newMap: Map[Point, Int] = acc + (current -> (value + 1))
              if (value >= 9) {
                (current :: newFlashes, newMap)
              } else {
                (newFlashes, newMap)
              }
            case _ =>
              (newFlashes, acc)
          }
      }
    }

    @tailrec
    private def flashStep(
        octopi: Map[Point, Int],
        alreadyFlashed: Set[Point],
        remaining: List[Point]
    ): (Long, Map[Point, Int]) = {
      remaining match {
        case Nil => (alreadyFlashed.size, octopi)
        case next :: rest if alreadyFlashed.contains(next) =>
          flashStep(octopi, alreadyFlashed, rest)
        case next :: rest =>
          val (newFlashes, newMap) = flash(octopi, next)
          val seen                 = alreadyFlashed + next
          flashStep(
            newMap,
            seen,
            newFlashes.filter(x => !seen.contains(x)) ++ remaining
          )
      }
    }

    val step: OctoState[Long] = {
      State { octoState =>
        val increasedEnergy   = octoState.mapValues(_ + 1).toMap
        val remaining         = increasedEnergy.toList.collect { case (octopus, energy) if energy > 9 => octopus }
        val (flashed, newMap) = flashStep(increasedEnergy, Set.empty, remaining)
        (newMap, flashed)
      }
    }

    val untilFullFlash: OctoState[Int] = {
      def untilFullFlash(idx: Int): OctoState[Int] = {
        for {
          initial <- State.get[Map[Point, Int]]
          stepped <- step
          after = idx + 1
          next <-
            if (initial.size == stepped) {
              after.pure[OctoState]
            } else {
              untilFullFlash(after)
            }
        } yield next
      }
      untilFullFlash(0)
    }
  }

  private def loadInput(path: String): Map[Point, Int] = {
    val lines = Util.loadResource(path)

    lines.foldLeft[(Int, Map[Point, Int])]((0, Map.empty)) { case ((y, acc), line) =>
      val xs = line.toCharArray.toList.foldLeft[(Int, Map[Point, Int])]((0, acc)) { case ((x, acc), elem) =>
        (x + 1, acc + ((x, y) -> elem.toString.toInt))
      }
      (y + 1, xs._2)
    }._2
  }
}
