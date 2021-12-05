package dk.alfabetacain.adventofcode._2021.shared

import scala.util.Try

object Parsers {
  import fastparse._, NoWhitespace._

  def spaces[_: P]: P[Unit] = P(CharsWhileIn(" \t\r\n"))

  def int[_: P]: P[Int] =
    P(CharsWhileIn("0-9").!).filter(maybeNumber => Try(maybeNumber.toInt).toOption.isDefined).map(_.toInt)

  def comma[_: P]: P[Unit] = P(",")

  def arrow[_: P]: P[Unit] = P("->")
}
