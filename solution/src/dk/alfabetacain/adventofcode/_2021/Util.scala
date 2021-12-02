package dk.alfabetacain.adventofcode._2021

import scala.io.Source

object Util {

  def loadResource(path: String): List[String] = {
    Source.fromResource(path).getLines.toList
  }
}
