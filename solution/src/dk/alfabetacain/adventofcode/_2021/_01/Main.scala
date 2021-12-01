package dk.alfabetacain.adventofcode._2021._01

import cats.effect.IOApp
import cats.effect.{ ExitCode, IO }
import scala.io.Source

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {

    for {
      _ <- solveFirst()
      _ <- solveSecond()
    } yield ExitCode.Success
  }

  private def solveFirst(): IO[Unit] = {
    IO {
      val inputs = Source.fromResource("01/1.txt").getLines.toList.map(_.toInt)

      val (res, _) = inputs.foldLeft[(Int, Option[Int])]((0, None)) { case ((count, previous), current) =>
        val increase = previous.filter(_ < current).map(_ => 1).getOrElse(0)
        (count + increase, Some(current))
      }

      println(s"First = $res")
    }
  }

  private def solveSecond(): IO[Unit] = {
    IO {
      val inputs = Source.fromResource("01/1.txt").getLines.toList.map(_.toInt).sliding(3)

      val (res, _) = inputs.foldLeft[(Int, Option[Int])]((0, None)) { case ((count, previous), current) =>
        val currentSum = current.sum
        val increase   = previous.filter(_ < currentSum).map(_ => 1).getOrElse(0)
        (count + increase, Some(currentSum))
      }

      println(s"Second = $res")
    }
  }
}
