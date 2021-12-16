package dk.alfabetacain.adventofcode._2021._15

import cats.implicits._
import cats.syntax._
import cats.effect.IOApp
import cats.effect.{ ExitCode, IO }
import dk.alfabetacain.adventofcode._2021.Util
import cats.kernel.Monoid
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable
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

  type Point = (Int, Int)
  type Cave  = Map[Point, Int]

  private def adjacent(point: Point, cave: Cave): List[Point] = {
    List(
      (-1, 0),
      (1, 0),
      (0, -1),
      (0, 1),
    ).map(Monoid[Point].combine(point, _)).filter(cave.contains)
  }

  @tailrec
  private def path(acc: List[Point], cameFrom: mutable.Map[Point, Point], end: Point): List[Point] = {
    cameFrom.get(end) match {
      case None => end :: acc
      case Some(before) =>
        path(end :: acc, cameFrom, before)
    }
  }

  private def aStar(cave: Cave, start: Point, target: Point): List[Point] = {

    type QueueEntry = (Point, Int)
    val queue = new PriorityQueue[QueueEntry]()(Ordering.by(-1 * _._2))
    queue.enqueue((start, -1))
    val costs = mutable.Map[Point, Int]()
    costs.put(start, 0)
    val cameFrom = mutable.Map[Point, Point]()

    @tailrec
    def rec(): List[Point] = {
      val current = queue.dequeue()
      if (current._1 == target) {
        // println("done!")
        path(List.empty, cameFrom, target)
      } else if (current._2 > costs(current._1)) {
        // println(s"ignoring $current")
        rec()
      } else {
        // println(s"Handling $current")
        val neighbors = adjacent(current._1, cave)
        neighbors.foreach { neighbor =>
          val score = costs(current._1) + cave(neighbor)
          if (score < costs.getOrElse(neighbor, Int.MaxValue)) {
            cameFrom.put(neighbor, current._1)
            costs.put(neighbor, score)
            queue.enqueue((neighbor, score))
          }
        }
        rec()
      }
    }
    rec()
  }

  private def solveFirstTest(): IO[Unit] = {
    IO {
      val (cave, target) = load("15/1_test.txt")
      val path           = aStar(cave, (0, 0), target)
      println(s"Path = \n${path.mkString("\n")}")
      val pathCost = path.map(cave.apply).sum - cave((0, 0))
      println(s"First test = $pathCost")
    }
  }

  private def solveFirst(): IO[Unit] = {
    IO {
      val (cave, target) = load("15/1.txt")
      val path           = aStar(cave, (0, 0), target)
      // println(s"Path = \n${path.mkString("\n")}")
      val pathCost = path.map(cave.apply).sum - cave((0, 0))
      println(s"First = $pathCost")
    }
  }

  private def solveSecondTest(): IO[Unit] = {
    IO {
      val (cave, target) = load2("15/1_test.txt")
      val path           = aStar(cave, (0, 0), target)
      // println(s"Path = \n${path.mkString("\n")}")
      val pathCost = path.map(cave.apply).sum - cave((0, 0))
      println(s"Second test = $pathCost")
    }
  }

  private def solveSecond(): IO[Unit] = {
    IO {
      val (cave, target) = load2("15/1.txt")
      val path           = aStar(cave, (0, 0), target)
      // println(s"Path = \n${path.mkString("\n")}")
      path.foldLeft[Point]((0, 0)){ (previous, current) =>
        if (current._1 < previous._1 || current._2 < previous._2) {
          println("backtracking!")
        }
        current
      }
      val pathCost = path.map(cave.apply).sum - cave((0, 0))
      println(s"Second = $pathCost")
    }
  }

  private def displace(cave: Cave)(pointDisplace: Point => Point, costDisplace: Int => Int): Cave = {
    val newMap = cave.map { case (point, cost) => (pointDisplace(point), costDisplace(cost)) }
    println(s"newMap size = ${newMap.size}")
    newMap
  }

  private def mergeCaves(left: Cave, right: Cave): Cave = {
    left.foldLeft[Cave](right) { case (acc, (key, value)) =>
      if (acc.contains(key)) {
        println(s"Map already contains $key")
        ???
      } else {
        acc + (key -> value)
      }
    }
  }

  private def load2(path: String): (Cave, Point) = {
    val lines = Util.loadResource(path)
    val maps = for {
      (line, y) <- lines.zipWithIndex
      (cost, x) <- line.trim.toCharArray.map(_.toString.toInt).zipWithIndex
    } yield Map((x, y) -> cost)

    val target       = maps.last.keySet.head
    val baseCave     = Monoid[Cave].combineAll(maps)
    val baseCaveSide = lines.head.trim.size
    val additions: List[Cave] =
      for {
        y <- (0 to 4).toList
        x <- (0 to 4).toList
        if x > 0 || y > 0
      } yield displace(baseCave)(
        { point => (point._1 + x * baseCaveSide, point._2 + y * baseCaveSide) },
        { cost =>
          val newCost = cost + x + y
          if (newCost > 9) {
            1 + newCost % 10
          } else {
            newCost
          }
        }
      )
    /*
    additions.foldLeft[Cave](baseCave) { (acc, current) =>
      println(s"merging $current")
      mergeCaves(acc, current)
    }
     */
    println(s"additions = ${additions.map(_.size).sum}")
    val all = Monoid[Cave].combineAll(baseCave :: additions)
    println(s"All size = ${all.size}")
    if (all.size != baseCave.size * 25) {
      println(s"Got size ${all.size}, expected ${baseCave.size * 25}")
      ???
    }
    val max = all.maxBy(x => x._1._1 + x._1._2)._1
    println(s"Max cost = ${all(max)}")
    // val printed =
    // all.toList.groupBy(_._1._2).toList.sortBy(_._1).map(_._2.sortBy(_._1._1)).map(_.map(_._2).mkString).mkString("\n")
    // println(printed)
    (all, max)
  }

  private def load(path: String): (Cave, Point) = {
    val lines = Util.loadResource(path)
    val maps = for {
      (line, y) <- lines.zipWithIndex
      (cost, x) <- line.trim.toCharArray.map(_.toString.toInt).zipWithIndex
    } yield Map((x, y) -> cost)

    val target = maps.last.keySet.head
    (Monoid[Cave].combineAll(maps), target)
  }
}
