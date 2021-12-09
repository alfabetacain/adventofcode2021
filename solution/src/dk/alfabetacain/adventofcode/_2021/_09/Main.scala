package dk.alfabetacain.adventofcode._2021._09

import cats.implicits._
import cats.syntax._
import cats.effect.IOApp
import cats.effect.{ ExitCode, IO }
import dk.alfabetacain.adventofcode._2021.Util
import scala.annotation.tailrec

object Main extends IOApp {

  type Terrain = Array[Array[Int]]

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
      val input = loadInput("09/1_test.txt")

      val points = input.zipWithIndex.flatMap { case (row, i) =>
        row.zipWithIndex.map { case (point, j) =>
          if (isLowest(input, j, i)) {
            Some((j, i))
          } else {
            None
          }
        }.collect { case Some(v) => v }
      }

      val result = points.map { case (x, y) => input(y)(x) + 1 }.sum
      println(s"First test = $result")
    }
  }

  private def isLowest(map: Array[Array[Int]], x: Int, y: Int): Boolean = {
    val current          = map(y)(x)
    val isLowerThanLeft  = x < 1 || current < map(y)(x - 1)
    val isLowerThanRight = x >= map(y).length - 1 || current < map(y)(x + 1)

    val isLowerThanUp   = y < 1 || current < map(y - 1)(x)
    val isLowerThanDown = y >= map.length - 1 || current < map(y + 1)(x)

    isLowerThanLeft && isLowerThanUp && isLowerThanRight && isLowerThanDown
  }

  private def findLowestPoints(map: Array[Array[Int]]): List[(Int, Int)] = {

    map.zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.map { case (point, x) =>
        if (isLowest(map, x, y)) {
          Some((x, y))
        } else {
          None
        }
      }.collect { case Some(v) => v }
    }.toList
  }

  private def spread(terrain: Terrain, x: Int, y: Int, previous: Set[(Int, Int)]): Set[(Int, Int)] = {
    if (terrain(y)(x) == 9) {
      Set.empty
    } else {
      val self             = (x, y)
      val selfValue        = terrain(y)(x)
      val previousWithSelf = previous + self

      val left =
        if (x < 1) {
          Set.empty
        } else {
          val leftCoords = (x - 1, y)
          if (!previous.contains(leftCoords) && selfValue < terrain(leftCoords._2)(leftCoords._1)) {
            spread(terrain, x - 1, y, previousWithSelf)
          } else {
            Set.empty
          }
        }

      val right =
        if (x >= terrain(y).length - 1) {
          Set.empty
        } else {
          val rightCoords = (x + 1, y)
          if (!previous.contains(rightCoords) && selfValue < terrain(rightCoords._2)(rightCoords._1)) {
            spread(terrain, x + 1, y, previousWithSelf)
          } else {
            Set.empty
          }
        }

      val up =
        if (y < 1) {
          Set.empty
        } else {
          val upCoords = (x, y - 1)
          if (!previous.contains(upCoords) && selfValue < terrain(upCoords._2)(upCoords._1)) {
            spread(terrain, x, y - 1, previousWithSelf)
          } else {
            Set.empty
          }
        }

      val down =
        if (y >= terrain.length - 1) {
          Set.empty
        } else {
          val downCoords = (x, y + 1)
          if (!previous.contains(downCoords) && selfValue < terrain(downCoords._2)(downCoords._1)) {
            spread(terrain, x, y + 1, previousWithSelf)
          } else {
            Set.empty
          }
        }

      previousWithSelf ++ left ++ right ++ up ++ down
    }
  }

  private def solveFirst(): IO[Unit] = {

    IO {
      val input = loadInput("09/1.txt")

      val points = input.zipWithIndex.flatMap { case (row, i) =>
        row.zipWithIndex.map { case (point, j) =>
          if (isLowest(input, j, i)) {
            Some((j, i))
          } else {
            None
          }
        }.collect { case Some(v) => v }
      }

      val result = points.map { case (x, y) => input(y)(x) + 1 }.sum
      println(s"First = $result")
    }
  }

  private def solveSecondTest(): IO[Unit] = {
    IO {
      val terrain      = loadInput("09/1_test.txt")
      val lowestPoints = findLowestPoints(terrain)

      val lowestThree = lowestPoints.map { point =>
        spread(terrain, point._1, point._2, Set.empty)
      }.filter(_.nonEmpty).sortBy(_.size * -1).take(3)

      val result = lowestThree.map(_.size).foldLeft(1)(_ * _)
      println(s"Second test = $result")
    }
  }

  private def solveSecond(): IO[Unit] = {
    IO {
      val terrain      = loadInput("09/1.txt")
      val lowestPoints = findLowestPoints(terrain)

      val lowestThree = lowestPoints.map { point =>
        spread(terrain, point._1, point._2, Set.empty)
      }.filter(_.nonEmpty).sortBy(_.size * -1).take(3)

      val result = lowestThree.map(_.size).foldLeft(1)(_ * _)
      println(s"Second = $result")
    }
  }

  private def loadInput(path: String): Terrain = {
    val input = Util.loadResource(path)

    input.map { line =>
      line.toCharArray.map(_.toString.toInt)
    }.toArray
  }
}
