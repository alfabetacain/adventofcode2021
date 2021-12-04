package dk.alfabetacain.adventofcode._2021._04

import cats.effect.IOApp
import cats.effect.{ ExitCode, IO }
import dk.alfabetacain.adventofcode._2021.Util
import cats.data.State
import cats.implicits._
import cats.syntax._
import scala.annotation.tailrec
import cats.Eval

object Main extends IOApp {

  final case class Board(input: List[List[(Int, Boolean)]]) {

    def mark(number: Int): Board = {
      val newInput = input.map { row =>
        row.map { current =>
          if (current._1 == number) {
            (current._1, true)
          } else {
            current
          }
        }
      }
      this.copy(input = newInput)
    }

    def calculateScore(winningNumber: Int): Int = {
      val unmarkedSum = input.map { row =>
        row.map { current =>
          if (!current._2) {
            current._1
          } else {
            0
          }
        }.sum
      }.sum
      unmarkedSum * winningNumber
    }

    private def hasRowWon(input: List[(Int, Boolean)]): Boolean =
      input.forall(_._2)

    private def hasColumnWon(idx: Int): Boolean = {
      input.map { row =>
        row(idx)._2
      }.forall(identity)
    }

    def hasWon(): Boolean = {
      input.exists(hasRowWon) || (Range(0, 5)).exists(hasColumnWon)
    }

    override def toString(): String = {
      input.map { row =>
        row.map { number =>
          val suffix =
            if (number._2) {
              "!"
            } else {
              " "
            }
          f"${number._1}%2d${suffix}"
        }.mkString(" ")
      }.mkString("\n", "\n", "\n")
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      _ <- solveFirstTest()
      _ <- solveFirst()
      _ <- solveSecondTest()
      _ <- solveSecond()
    } yield ExitCode.Success
  }

  private def play(number: Int): State[List[Board], (Int, Option[Board])] = {
    for {
      state <- State.get[List[Board]]
      updated = state.map(_.mark(number))
      _ <- State.set(updated)
      winner = state.zip(updated).flatMap { case (oldBoard, newBoard) =>
        if (oldBoard.hasWon != newBoard.hasWon) {
          List(newBoard)
        } else {
          List.empty
        }
      }.headOption
    } yield (number, winner)
  }

  private def foldWhile[A, B](
      input: List[State[A, B]],
      boards: A
  )(pred: (A, B) => Boolean): Eval[B] = {
    input match {
      case h :: t =>
        h.run(boards).flatMap { case (newState, result) =>
          if (pred(newState, result)) {
            foldWhile(t, newState)(pred)
          } else {
            result.pure[Eval]
          }
        }
      case Nil => ???
    }
  }

  private def run(path: String)(pred: (List[Board], (Int, Option[Board])) => Boolean): Int = {
    val (numbers, boards) = loadInput(path)

    val seq = numbers.map(play)

    val (winningNumber, Some(winningBoard)) = foldWhile(seq, boards)(pred).value
    winningBoard.calculateScore(winningNumber)
  }

  private def solveFirstTest(): IO[Unit] = {
    IO {
      val result = run("04/1_test.txt") {
        case (_, (_, maybeWinner)) =>
          maybeWinner.isEmpty
      }
      println(s"First test = $result")
    }
  }

  private def solveFirst(): IO[Unit] = {
    IO {
      val result = run("04/1.txt") {
        case (_, (_, maybeWinner)) =>
          maybeWinner.isEmpty
      }
      println(s"First = $result")
    }
  }

  private def solveSecondTest(): IO[Unit] = {
    IO {
      val result = run("04/1_test.txt") {
        case (state, (_, maybeWinner)) =>
          maybeWinner.isEmpty || state.exists(!_.hasWon)
      }
      println(s"First = $result")
    }
  }

  private def solveSecond(): IO[Unit] = {
    IO {
      val result = run("04/1.txt") {
        case (state, (_, maybeWinner)) =>
          maybeWinner.isEmpty || state.exists(!_.hasWon)
      }
      println(s"First = $result")
    }
  }

  private def loadInput(path: String): (List[Int], List[Board]) = {
    val input   = Util.loadResource(path)
    val numbers = input.head.split(",").filter(_.trim.nonEmpty).map(_.trim.toInt).toList

    val boards = readBoard(input.tail, List.empty)
    (numbers, boards)
  }

  private def parseBoardLine(input: String): List[(Int, Boolean)] = {
    input.split(" ").filter(_.trim.nonEmpty).map(_.trim.toInt).map((_, false)).toList
  }

  @tailrec
  private def readBoard(input: List[String], acc: List[Board]): List[Board] = {
    def doRead(input: List[String]): Option[Board] = {
      if (input.size == 6) {
        Board(input.drop(1).map(parseBoardLine)).some
      } else {
        None
      }
    }

    doRead(input.take(6)) match {
      case None => acc.reverse
      case Some(board) =>
        readBoard(input.drop(6), board :: acc)
    }
  }
}
