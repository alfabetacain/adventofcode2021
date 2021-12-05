package dk.alfabetacain.adventofcode._2021._05

import cats.syntax._
import cats.implicits._
import cats.Monoid
import cats.effect.IOApp
import cats.effect.{ ExitCode, IO }
import fastparse._, NoWhitespace._
import dk.alfabetacain.adventofcode._2021.shared.Parsers
import dk.alfabetacain.adventofcode._2021.Util

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      _ <- solveFirstTest()
      _ <- solveFirst()
      _ <- solveSecondTest()
      _ <- solveSecond()
    } yield ExitCode.Success
  }

  private def coordinateParser[_: P]: P[(Int, Int)] = P(Parsers.int ~ Parsers.comma ~ Parsers.int)

  private def lineParser[_: P]: P[((Int, Int), (Int, Int))] = P(
    coordinateParser ~ Parsers.spaces ~ Parsers.arrow ~ Parsers.spaces ~ coordinateParser
  ).map(result => ((result._1, result._2), (result._3._1, result._3._2)))

  private def range(from: Int, to: Int): Range.Inclusive = {
    val step =
      if (from < to) {
        1
      } else {
        -1
      }
    Range.inclusive(from, to, step)
  }

  private def coordinateEntry(x: Int, y: Int): String = s"$x:$y"

  private def is45DegreesDiagonal(from: (Int, Int), to: (Int, Int)): Boolean = {
    Math.abs(from._1 - to._1) == Math.abs(from._2 - to._2)
  }

  private def parseLine(line: String, supportsDiagonal: Boolean): Map[String, Int] = {
    val opt = parse(line.trim(), lineParser(_)) match {
      case _: Parsed.Failure =>
        println(s"failed to parse '${line}'")
        None
      case Parsed.Success(res, _) =>
        Some(res)
    }
    opt.map { case (from, to) =>
      if (from._1 == to._1) {
        val yRange = range(from._2, to._2)
        yRange.map(y => coordinateEntry(from._1, y) -> 1).toMap

      } else if (from._2 == to._2) {
        val xRange = range(from._1, to._1)
        xRange.map(x => coordinateEntry(x, from._2) -> 1).toMap

      } else if (supportsDiagonal && is45DegreesDiagonal(from, to)) {
        val xRange = range(from._1, to._1)
        val yRange = range(from._2, to._2)
        xRange.zip(yRange).map { case (x, y) => coordinateEntry(x, y) -> 1 }.toMap

      } else {
        Map.empty[String, Int]
      }
    }.getOrElse(Map.empty)
  }

  private def loadInput(path: String, supportsDiagonal: Boolean): Map[String, Int] = {
    val input = Util.loadResource(path).map(parseLine(_, supportsDiagonal))
    input.foldLeft[Map[String, Int]](Monoid[Map[String, Int]].empty)(Monoid.combine)
  }

  private def solveFirstTest(): IO[Unit] = {
    IO {
      val input               = loadInput("05/1_test.txt", false)
      val numberOfOverlapping = input.values.count(_ > 1)
      println(s"First test = $numberOfOverlapping")
    }
  }

  private def solveFirst(): IO[Unit] = {
    IO {
      val input               = loadInput("05/1.txt", false)
      val numberOfOverlapping = input.values.count(_ > 1)
      println(s"First test = $numberOfOverlapping")
    }
  }

  private def solveSecondTest(): IO[Unit] = {
    IO {
      val input               = loadInput("05/1_test.txt", true)
      val numberOfOverlapping = input.values.count(_ > 1)
      println(s"First test = $numberOfOverlapping")
    }
  }

  private def solveSecond(): IO[Unit] = {
    IO {
      val input               = loadInput("05/1.txt", true)
      val numberOfOverlapping = input.values.count(_ > 1)
      println(s"First test = $numberOfOverlapping")
    }
  }
}
