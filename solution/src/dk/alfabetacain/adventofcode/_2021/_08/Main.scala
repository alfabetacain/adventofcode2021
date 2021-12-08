package dk.alfabetacain.adventofcode._2021._08

import cats.syntax._
import cats.implicits._
import cats.effect.IOApp
import cats.effect.{ ExitCode, IO }
import dk.alfabetacain.adventofcode._2021.Util
import fastparse._, fastparse.NoWhitespace._
import dk.alfabetacain.adventofcode._2021.shared.Parsers
import fastparse.Parsed.Success
import fastparse.Parsed.Failure
import scala.annotation.tailrec

object Main extends IOApp {

  type Digit = String

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      _ <- solveFirstTest()
      _ <- solveFirst()
      _ <- solveSecondTest()
      _ <- solveSecond()
    } yield ExitCode.Success
  }

  private def isUnique(digits: Digit): Boolean = {
    digits.size == 2 || digits.size == 3 || digits.size == 4 || digits.size == 7
  }

  private def solveFirstTest(): IO[Unit] = {
    IO {
      val input  = loadInput("08/1_test.txt")
      val result = input.map(digits => digits.count(isUnique)).sum
      println(s"First test = $result")
    }
  }

  private def solveFirst(): IO[Unit] = {
    IO {
      val input  = loadInput("08/1.txt")
      val result = input.map(digits => digits.count(isUnique)).sum
      println(s"First = $result")
    }
  }

  private def toNumber(digit: Digit): Long = {
    digit.sorted match {
      case "abcefg"  => 0
      case "cf"      => 1
      case "acdeg"   => 2
      case "acdfg"   => 3
      case "bcdf"    => 4
      case "abdfg"   => 5
      case "abdefg"  => 6
      case "acf"     => 7
      case "abcdefg" => 8
      case "abcdfg"  => 9
      case other     =>
        // println(s"got unknown digit: $other")
        -1
    }
  }

  private def replaceWithSolution(input: Digit, solution: Map[String, String]): Digit = {
    input.toCharArray.map { char =>
      solution(char.toString)
    }.mkString
  }

  private def solveSecondTest(): IO[Unit] = {
    IO {
      println("----------------------------------------------")
      val input = loadInput2("08/1_test.txt")
      val result = input.map(x => (x._1.map(_.sorted), x._2.map(_.sorted))).map { digits =>
        val solution = solveLine(digits._1 ++ digits._2)
        // println("using solution:")
        // ppp(solution.toList.map(x => (x._1, Set(x._2))))
        // println(s"digits 2 = ${digits._2}")
        val mapped = digits._2.map { digit => replaceWithSolution(digit, solution) }.map(toNumber)
        mapped.mkString.toLong
      }.sum
      println(s"Second test = $result")
    }
  }

  private def solveSecond(): IO[Unit] = {
    IO {

      println("----------------------------------------------")
      val input = loadInput2("08/1.txt")
      val result = input.map(x => (x._1.map(_.sorted), x._2.map(_.sorted))).map { digits =>
        val solution = solveLine(digits._1 ++ digits._2)
        // println("using solution:")
        // ppp(solution.toList.map(x => (x._1, Set(x._2))))
        // println(s"digits 2 = ${digits._2}")
        val mapped = digits._2.map { digit => replaceWithSolution(digit, solution) }.map(toNumber)
        mapped.mkString.toLong
      }.sum
      println(s"Second = $result")
    }
  }

  private val all: Set[String] = Set("a", "b", "c", "d", "e", "f", "g")

  type Facts = Map[String, Set[String]]

  private val initial: Facts = Map(
    "a" -> all,
    "b" -> all,
    "c" -> all,
    "d" -> all,
    "e" -> all,
    "f" -> all,
    "g" -> all
  )

  def constrain(facts: Facts, key: String, value: Set[String]): Facts = {
    facts + (key -> facts(key).intersect(value))
  }

  private def pp(facts: Facts): Unit = {
    val s = facts.toList.map(v => (v._1, v._2.mkString(", "))).map(v => s"${v._1} -> ${v._2}").mkString("\n")
    println(s)
  }

  private def ppp(facts: List[(String, Set[String])]): Unit = {
    val s = facts.toList.map(v => (v._1, v._2.mkString(", "))).map(v => s"${v._1} -> ${v._2}").mkString("\n")
    println(s)
  }

  private def doSolve(
      controlSequence: List[Digit],
      facts: List[(String, Set[String])],
      input: List[Char]
  ): Option[List[(String, String)]] = {
    // println(s"Left = $input\nFacts =")
    // ppp(facts)
    input match {
      case h :: t =>
        for {
          fact <- facts.find(_._1 == h.toString)
          solution <- fact._2.flatMap { chosen =>
            val updatedFacts: List[(String, Set[String])] = facts.map { fact =>
              if (fact._1 == h.toString) {
                (h.toString, Set(chosen))
              } else {
                (fact._1, fact._2.filter(_ != chosen))
              }
            }
            if (updatedFacts.exists(_._2.isEmpty)) {
              None
            } else {
              doSolve(controlSequence, updatedFacts, t)
            }
          }.headOption
        } yield solution
      // choose value for h
      case Nil =>
        // check if it is valid
        val res = facts.map { fact => fact._2.headOption.map(res => (fact._1, res)) }.sequence.filter { solution =>
          controlSequence.map(replaceWithSolution(_, solution.toMap)).map(toNumber).forall(_ >= 0)
        }
        if (res.isDefined) {
          // println("found solution")
          // ppp(res.get.map(x => (x._1, Set(x._2))))
        } else {
          // println("no solution. Backtracking...")
        }
        res
    }
  }

  private def solveLine(input: List[Digit]): Map[String, String] = {

    @tailrec
    def go(facts: Facts, input: List[Digit]): Facts = {
      val nextSolution = input.foldLeft(facts) { (acc, elem) =>
        val size = elem.size
        if (size == 2) {
          elem.toCharArray.foldLeft(acc) { (acc, elem) => constrain(acc, elem.toString, Set("c", "f")) }
        } else if (size == 3) {
          elem.toCharArray.foldLeft(acc) { (acc, elem) => constrain(acc, elem.toString, Set("a", "c", "f")) }
        } else if (size == 4) {
          elem.toCharArray.foldLeft(acc) { (acc, elem) => constrain(acc, elem.toString, Set("b", "c", "d", "f")) }
        } else {
          acc
        }
      }
      if (facts == nextSolution) {
        // println("No change. Ending...")
        facts
      } else {
        // println("Changed from:")
        // pp(facts)
        // println("to:")
        // pp(nextSolution)
        // println("continuing...")
        go(nextSolution, input)
      }
    }

    // println("------------------------")
    // println(s"solving line '${input.mkString(" ")}'")
    val res = go(initial, input)
    doSolve(input, res.toList.sortBy(_._1), "abcdefg".toCharArray.toList) match {
      case Some(value) => value.toMap
      case None =>
        println(s"no solution for $input")
        ???
    }
    /*
    res.mapValues { set =>
      if (set.size > 1) {
        println(s"too many options: ${set.mkString(", ")}")
        ???
      } else {
        set.head
      }
    }.toMap
     */
  }

  private def digitParser[_: P] = P(CharsWhileIn("a-g"))

  private def sequenceParser[_: P]: P[Seq[String]] = digitParser.!.rep(10)

  private def lineParser[_: P]: P[(Seq[String], Seq[String])] = {
    P(
      digitParser.!.rep(exactly = 10, sep = " ") ~ Parsers.spaces.? ~ "|" ~ Parsers.spaces ~ digitParser.!.rep(
        exactly = 4,
        sep = " "
      )
    )
  }

  private def loadInput2(path: String): List[(List[Digit], List[Digit])] = {
    Util.loadResource(path).map { line =>
      parse(line, lineParser(_)) match {
        case Success(value, index) =>
          Some((value._1.toList, value._2.toList))
        case _: Failure =>
          None
      }
    }.collect { case Some(v) => v }
  }

  private def loadInput(path: String): List[List[Digit]] = {
    Util.loadResource(path).map { line =>
      parse(line, lineParser(_)) match {
        case Success(value, index) =>
          value._2.toList
        case _: Failure =>
          List.empty
      }
    }.filter(_.nonEmpty)
  }
}
