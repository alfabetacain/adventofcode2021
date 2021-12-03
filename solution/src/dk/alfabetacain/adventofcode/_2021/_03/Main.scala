package dk.alfabetacain.adventofcode._2021._03

import cats.effect.IOApp
import cats.effect.{ ExitCode, IO }
import dk.alfabetacain.adventofcode._2021.Util
import scala.annotation.tailrec

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      _ <- solveFirstTest()
      _ <- solveFirst()
      _ <- solveSecondTest()
      _ <- solveSecond()
    } yield ExitCode.Success
  }

  private def mostCommon[A](input: List[A]): A = {
    val counts = input.groupMapReduce(identity)(_ => 1)(_ + _)
    counts.toList.sortBy(_._2).reverse.maxBy(_._2)._1
  }

  private def leastCommon[A](input: List[A]): A = {
    val counts = input.groupMapReduce(identity)(_ => 1)(_ + _)
    counts.toList.sortBy(_._2).minBy(_._2)._1
  }

  private def solveFirstTest(): IO[Unit] = {
    IO {
      val input = Util.loadResource("03/1_test.txt").map(_.toCharArray.toList).transpose

      val gamma   = Integer.parseInt(input.map(mostCommon).mkString, 2)
      val epsilon = Integer.parseInt(input.map(leastCommon).mkString, 2)

      println(s"First test = ${gamma * epsilon}")
    }
  }

  private def solveFirst(): IO[Unit] = {
    IO {
      val input = Util.loadResource("03/1.txt").map(_.toCharArray.toList).transpose

      val gamma   = Integer.parseInt(input.map(mostCommon).mkString, 2)
      val epsilon = Integer.parseInt(input.map(leastCommon).mkString, 2)

      println(s"First test = ${gamma * epsilon}")
    }
  }

  private def solveSecondTest(): IO[Unit] = {
    IO {
      val input = Util.loadResource("03/1_test.txt")

      val o2Rating  = Integer.parseInt(solve(mostCommon)(input, 0), 2)
      val co2Rating = Integer.parseInt(solve(leastCommon)(input, 0), 2)

      println(s"Second test = ${o2Rating * co2Rating}")
    }
  }

  @tailrec
  private def solve(finder: List[Char] => Char)(input: List[String], idx: Int): String = {
    val currentPosition = input.map(_.charAt(idx))
    val wantedKey       = finder(currentPosition)
    input.filter(_.charAt(idx) == wantedKey) match {
      case solution :: Nil =>
        solution
      case Nil => ???
      case remaining =>
        solve(finder)(remaining, idx + 1)
    }
  }

  private def solveSecond(): IO[Unit] = {
    IO {
      val input = Util.loadResource("03/1.txt")

      val o2Rating  = Integer.parseInt(solve(mostCommon)(input, 0), 2)
      val co2Rating = Integer.parseInt(solve(leastCommon)(input, 0), 2)

      println(s"Second = ${o2Rating * co2Rating}")
    }
  }
}
