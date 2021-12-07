package dk.alfabetacain.adventofcode._2021._07

import cats.effect.IOApp
import cats.effect.{ ExitCode, IO }
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

  private def constantDist(target: Long, crabs: List[Long]): Long = {
    crabs.map(crab => Math.abs(target - crab)).sum
  }

  private def increasingDist(target: Long, crabs: List[Long]): Long = {
    crabs.map {
      crab =>
        val dist = Math.abs(target - crab)
        (dist * (dist + 1)) / 2
    }.sum
  }

  private def solveFirstTest(): IO[Unit] = {
    IO {
      val input = loadInput("07/1_test.txt")
      val low   = input.min.toInt
      val high  = input.max.toInt

      val result = Range.inclusive(low, high).map(target => constantDist(target, input)).min
      println(s"First test = $result")
    }
  }

  private def solveFirst(): IO[Unit] = {
    IO {
      val input = loadInput("07/1.txt")
      val low   = input.min.toInt
      val high  = input.max.toInt

      val result = Range.inclusive(low, high).map(target => constantDist(target, input)).min
      println(s"First = $result")
    }
  }

  private def solveSecondTest(): IO[Unit] = {
    IO {
      val input = loadInput("07/1_test.txt")
      val low   = input.min.toInt
      val high  = input.max.toInt

      val result = Range.inclusive(low, high).map(target => increasingDist(target, input)).min
      println(s"Second test = $result")
    }
  }

  private def solveSecond(): IO[Unit] = {
    IO {

      val input = loadInput("07/1.txt")
      val low   = input.min.toInt
      val high  = input.max.toInt

      val result = Range.inclusive(low, high).map(target => increasingDist(target, input)).min
      println(s"Second = $result")
    }
  }

  private def loadInput(path: String): List[Long] = {
    Util.loadResource(path).flatMap { line =>
      line.split(",").map(_.trim).filter(_.nonEmpty).map(_.toLong)
    }.toList
  }
}
