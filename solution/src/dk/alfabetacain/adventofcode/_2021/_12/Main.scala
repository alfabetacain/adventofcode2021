package dk.alfabetacain.adventofcode._2021._12

import cats.syntax._
import cats.implicits._
import cats.effect.IOApp
import cats.effect.{ ExitCode, IO }
import dk.alfabetacain.adventofcode._2021.Util
import cats.kernel.Monoid
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

  private def solveFirstTest(): IO[Unit] = {
    IO {
      val paths = Graph.load("12/1_test.txt").bfs(0)
      println(s"First test = ${paths.size}")
    }
  }

  private def solveFirst(): IO[Unit] = {
    IO {
      val paths = Graph.load("12/1.txt").bfs(0)
      println(s"First = ${paths.size}")
    }
  }

  private def solveSecondTest(): IO[Unit] = {
    IO {
      val paths = Graph.load("12/1_test.txt").bfs(1)
      println(s"Second test = ${paths.size}")
    }
  }

  private def solveSecond(): IO[Unit] = {
    IO {
      val paths = Graph.load("12/1.txt").bfs(1)
      println(s"Second = ${paths.size}")
    }
  }

  trait Graph {
    def bfs(numberOfTwiceVisitsAllowed: Int): List[List[String]]
  }

  private class GraphImpl(graph: Map[String, Set[String]]) extends Graph {

    def bfs(
        numberOfTwiceVisitsAllowed: Int,
    ): List[List[String]] = {

      def doBfs(
          seen: Set[String],
          numberOfTwiceVisitsAllowed: Int,
          numberOfTwiceVisits: Int,
          graph: Map[String, Set[String]],
          path: List[String],
          current: String
      ): List[List[String]] = {

        val newPath = current :: path
        if (current == "end") {
          List(newPath)
        } else if (seen.contains(current) && numberOfTwiceVisits >= numberOfTwiceVisitsAllowed) {
          List.empty
        } else {
          val outgoing =
            graph(current).filter(x => !seen.contains(x) || numberOfTwiceVisits < numberOfTwiceVisitsAllowed)
          val newSeen =
            if (isSmall(current)) {
              seen + current
            } else {
              seen
            }
          outgoing.toList.flatMap(
            doBfs(
              newSeen,
              numberOfTwiceVisitsAllowed,
              numberOfTwiceVisits + (if (seen.contains(current)) 1 else 0),
              graph,
              newPath,
              _
            )
          ).filter(_.headOption == Some("end"))
        }
      }
      doBfs(Set.empty, numberOfTwiceVisitsAllowed, 0, graph, List.empty, "start")
    }
    private def isSmall(s: String): Boolean = s.head.isLower
  }

  object Graph {

    def load(path: String): Graph = {
      val input = Util.loadResource(path)
      val graph = Monoid[Map[String, Set[String]]].combineAll(
        input.map { line =>
          val parts = line.trim.split("-")
          val from  = parts(0)
          val to    = parts(1)
          List(
            Map(from -> Set(to)),
            Map(to   -> Set(from))
          )
        }.flatten
      ).mapValues(_.filter(_ != "start")).toMap
      new GraphImpl(graph)
    }
  }
}
