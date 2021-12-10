package dk.alfabetacain.adventofcode._2021._10

import cats.syntax._
import cats.implicits._
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

  private def isClosing(char: Char): Boolean = char match {
    case ')' => true
    case ']' => true
    case '}' => true
    case '>' => true
    case _   => false
  }

  private def isPair(left: Char, right: Char): Boolean = {
    (left, right) match {
      case ('(', ')') => true
      case ('[', ']') => true
      case ('{', '}') => true
      case ('<', '>') => true
      case _          => false
    }
  }

  @tailrec
  private def recurse(
      open: List[Char],
      wronglyClosed: List[(Char, Char)],
      input: List[Char]
  ): (List[Char], List[(Char, Char)]) = {
    input match {
      case Nil => (open, wronglyClosed)
      case h :: t if isClosing(h) =>
        open match {
          case lastOpen :: remainingOpen if isPair(lastOpen, h) =>
            recurse(remainingOpen, wronglyClosed, t)
          case lastOpen :: remainingOpen =>
            recurse(remainingOpen, (lastOpen, h) :: wronglyClosed, t)
          case Nil =>
            ???
        }
      case h :: t =>
        recurse(h :: open, wronglyClosed, t)
    }
  }

  private def score(char: Char): Long = {
    char match {
      case ')' => 3
      case ']' => 57
      case '}' => 1197
      case '>' => 25137
    }
  }

  private def solveFirstTest(): IO[Unit] = {
    IO {
      val input  = Util.loadResource("10/1_test.txt")
      val parsed = input.map(line => recurse(List.empty, List.empty, line.toCharArray.toList))

      val result = parsed.map { case (_, wronglyClosed) =>
        wronglyClosed.reverse match {
          case Nil => 0
          case h :: t =>
            score(h._2)
        }
      }.sum
      println(s"First test = $result")
    }
  }

  private def solveFirst(): IO[Unit] = {
    IO {
      val input  = Util.loadResource("10/1.txt")
      val parsed = input.map(line => recurse(List.empty, List.empty, line.toCharArray.toList))

      val result = parsed.map { case (_, wronglyClosed) =>
        wronglyClosed.reverse match {
          case Nil => 0
          case h :: t =>
            score(h._2)
        }
      }.sum
      println(s"First = $result")
    }
  }

  private def partner(char: Char): Char = {
    char match {
      case '('   => ')'
      case '['   => ']'
      case '{'   => '}'
      case '<'   => '>'
      case other => ???
    }
  }

  private def smallScore(char: Char): Long = {
    char match {
      case ')'   => 1
      case ']'   => 2
      case '}'   => 3
      case '>'   => 4
      case other => ???
    }
  }

  private def solveSecondTest(): IO[Unit] = {
    IO {
      val input  = Util.loadResource("10/1_test.txt")
      val parsed = input.map(line => recurse(List.empty, List.empty, line.toCharArray.toList))

      val incompleteSolutions = parsed.map { case (missingOpen, _) =>
        val solution = missingOpen.map(partner)
        solution.foldLeft(0L) { (acc, current) =>
          acc * 5 + smallScore(current)
        }
      }.filter(_ > 0L).sorted
      val middle = incompleteSolutions(incompleteSolutions.size / 2)
      println(s"Second test = $middle")
    }
  }

  private def solveSecond(): IO[Unit] = {
    IO {
      val input  = Util.loadResource("10/1.txt")
      val parsed = input.map(line => recurse(List.empty, List.empty, line.toCharArray.toList))

      val incompleteSolutions = parsed.filter(_._2.isEmpty).map { case (missingOpen, _) =>
        val solution = missingOpen.map(partner)
        solution.foldLeft(0L) { (acc, current) =>
          acc * 5 + smallScore(current)
        }
      }.filter(_ > 0L).sorted
      val middle = incompleteSolutions(incompleteSolutions.size / 2)
      println(s"Second = $middle")
    }
  }
}
