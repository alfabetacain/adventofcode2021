package dk.alfabetacain.adventofcode._2021._14

import cats.syntax._
import cats.implicits._
import cats.effect.IOApp
import cats.effect.{ ExitCode, IO }
import dk.alfabetacain.adventofcode._2021.Util
import cats.kernel.Monoid
import scala.annotation.tailrec
import java.util.{ HashMap => HMap }

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      _ <- solveFirstTest()
      _ <- solveFirst()
      _ <- solveSecondTest()
      _ <- solveSecond()
    } yield ExitCode.Success
  }

  type Instructions = Map[String, Char]

  private def solve(path: String, iterations: Int): Long = {

    val (input, productions) = load(path)

    def recurse(
        left: Char,
        right: Char,
        instructions: Instructions,
        cache: HMap[String, Map[Char, Long]],
        iterations: Int
    ): Map[Char, Long] = {
      if (iterations == 0) {
        Map.empty
      } else {
        val cacheKey = left.toString + right.toString + iterations
        cache.get(cacheKey) match {
          case null =>
            val middle     = instructions(left.toString + right.toString)
            val thisOne    = Map(middle -> 1L)
            val afterLeft  = recurse(left, middle, instructions, cache, iterations - 1)
            val afterRight = recurse(middle, right, instructions, cache, iterations - 1)
            val combined   = Monoid[Map[Char, Long]].combineAll(Seq(afterLeft, afterRight, thisOne))
            cache.put(cacheKey, combined)
            combined
          case v => v
        }
      }
    }
    val ls                                   = input.toCharArray.toList
    val baseFrequencies                      = Monoid[Map[Char, Long]].combineAll(ls.map(char => Map(char -> 1L)))
    val cache: HMap[String, Map[Char, Long]] = new HMap
    val merged = ls.sliding(2).foldLeft[Map[Char, Long]](baseFrequencies) { case (acc, left :: right :: _) =>
      Monoid[Map[Char, Long]].combine(acc, recurse(left, right, productions, cache, iterations))
    }

    val freqs = merged.toList
    freqs.maxBy(_._2)._2 - freqs.minBy(_._2)._2
  }

  private def solveFirstTest(): IO[Unit] = {
    IO {
      val result = solve("14/1_test.txt", 10)
      println(s"First test = $result")
    }
  }

  private def solveFirst(): IO[Unit] = {
    IO {
      val result = solve("14/1.txt", 10)
      println(s"First = $result")
    }
  }

  private def solveSecondTest(): IO[Unit] = {
    IO {
      val result = solve("14/1_test.txt", 40)
      println(s"Second test = $result")
    }
  }

  private def solveSecond(): IO[Unit] = {
    IO {
      val result = solve("14/1.txt", 40)
      println(s"Second = $result")
    }
  }

  private def load(path: String): (String, Instructions) = {
    val lines    = Util.loadResource(path)
    val template = lines.head.trim

    val instructions = lines.drop(2).map { line =>
      val parts = line.split("->")
      (parts(0).trim, parts(1).trim.head)
    }.toMap

    (template, instructions)
  }

}
