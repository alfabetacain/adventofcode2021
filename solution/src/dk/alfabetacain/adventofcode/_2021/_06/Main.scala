package dk.alfabetacain.adventofcode._2021._06

import cats.syntax._
import cats.implicits._
import cats.effect.IOApp
import cats.effect.{ ExitCode, IO }
import dk.alfabetacain.adventofcode._2021.Util
import cats.data.State
import cats.kernel.Monoid

object Main extends IOApp {

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
      val result = solve("06/1_test.txt", 80)
      println(s"First test = $result")
    }
  }

  private def solveFirst(): IO[Unit] = {
    IO {
      val result = solve("06/1.txt", 80)
      println(s"First = $result")
    }
  }

  private def solveSecondTest(): IO[Unit] = {
    IO {
      val result = solve("06/1_test.txt", 256)
      println(s"Second test = $result")
    }
  }

  private def solveSecond(): IO[Unit] = {
    IO {
      val result = solve("06/1.txt", 256)
      println(s"Second = $result")
    }
  }

  private def solve(path: String, ticks: Int): Long = {
    val input = loadInput(path)

    Range(0, ticks).map(_ => tick).toList.sequence.run(input).value._1.values.sum
  }

  private def tickFish(age: Long, numberOfFish: Long): Map[Long, Long] = {
    if (age == 0) {
      Map(6L -> numberOfFish, 8L -> numberOfFish)
    } else {
      Map((age - 1) -> numberOfFish)
    }
  }

  private def tick: State[Map[Long, Long], Unit] = {
    State.modify[Map[Long, Long]] { state =>
      Monoid[Map[Long, Long]].combineAll(
        state.map((tickFish _).tupled)
      )
    }
  }

  private def loadInput(path: String): Map[Long, Long] = {
    Util.loadResource(path).flatMap { line =>
      val parts = line.split(",")
      parts.map(_.trim).filter(_.nonEmpty).toList
    }.map(_.toLong).groupBy(identity).map(input => (input._1, input._2.size.toLong)).toMap
  }
}
