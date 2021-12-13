package dk.alfabetacain.adventofcode._2021._13

import cats.effect.IOApp
import cats.effect.{ ExitCode, IO }
import fastparse._, NoWhitespace._
import dk.alfabetacain.adventofcode._2021.shared.Parsers
import dk.alfabetacain.adventofcode._2021.Util
import fastparse.Parsed.Failure
import fastparse.Parsed.Success
import dk.alfabetacain.adventofcode._2021._13.Main.Instruction.FoldX
import dk.alfabetacain.adventofcode._2021._13.Main.Instruction.FoldY

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      _ <- solveFirstTest()
      _ <- solveFirst()
      _ <- solveSecond()
    } yield ExitCode.Success
  }

  type Point = (Int, Int)
  type Paper = Set[Point]
  sealed trait Instruction

  object Instruction {
    final case class FoldX(at: Int) extends Instruction
    final case class FoldY(at: Int) extends Instruction
  }

  private def fold(paper: Paper, instruction: Instruction): Paper = {
    instruction match {
      case FoldX(at) =>
        paper.map { point =>
          if (point._1 > at) {
            val newX = point._1 - (point._1 - at) * 2
            (newX, point._2)
          } else {
            point
          }
        }
      case FoldY(at) =>
        paper.map { point =>
          if (point._2 > at) {
            val newY = point._2 - (point._2 - at) * 2
            (point._1, newY)
          } else {
            point
          }
        }
    }
  }

  private def foldAll(paper: Paper, instructions: List[Instruction]): Paper = {
    instructions.foldLeft(paper) { (acc, current) => fold(acc, current) }
  }

  private def solveFirstTest(): IO[Unit] = {
    IO {
      val (paper, instructions) = load("13/1_test.txt")
      val afterFirstFold        = fold(paper, instructions.head)
      println(s"First test = ${afterFirstFold.size}")
    }
  }

  private def solveFirst(): IO[Unit] = {
    IO {
      val (paper, instructions) = load("13/1.txt")
      val afterFirstFold        = fold(paper, instructions.head)
      println(s"First = ${afterFirstFold.size}")
    }
  }

  private def render(paper: Paper): String = {
    val maxX = paper.map(_._1).max

    val maxY = paper.map(_._2).max

    Range.inclusive(0, maxY).map { y =>
      Range.inclusive(0, maxX).map { x =>
        if (paper.contains((x, y))) {
          "#"
        } else {
          "."
        }
      }.mkString(" ")
    }.mkString("\n")
  }

  private def solveSecond(): IO[Unit] = {
    IO {
      val (paper, instructions) = load("13/1.txt")
      val after                 = foldAll(paper, instructions)
      println(s"Second = \n${render(after)}")
    }
  }

  private def pointParser[_: P]: P[Point]              = P(Parsers.int ~ "," ~ Parsers.int)
  private def xInstructionParser[_: P]: P[Instruction] = P("fold along x=" ~ Parsers.int).map(Instruction.FoldX)
  private def yInstructionParser[_: P]: P[Instruction] = P("fold along y=" ~ Parsers.int).map(Instruction.FoldY)
  private def instructionParser[_: P]: P[Instruction]  = P(xInstructionParser | yInstructionParser)

  private def load(path: String): (Paper, List[Instruction]) = {
    val input = Util.loadResource(path)

    val (first, last) = input.splitAt(input.indexOf(""))

    val points = first.map { line =>
      parse(line, pointParser(_)) match {
        case _: Failure => None
        case Success(value, index) =>
          Some(value)
      }
    }.collect { case Some(v) => v }.toSet

    val instructions = last.map { line =>
      parse(line, instructionParser(_)) match {
        case _: Failure => None
        case Success(value, index) =>
          Some(value)
      }
    }.collect { case Some(v) => v }
    (points, instructions)
  }
}
