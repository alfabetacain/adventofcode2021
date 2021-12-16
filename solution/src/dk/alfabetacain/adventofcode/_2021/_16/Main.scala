package dk.alfabetacain.adventofcode._2021._16

import cats.syntax._
import cats.implicits._
import cats.effect.IOApp
import cats.effect.{ ExitCode, IO }
import scodec.bits._
import scodec.codecs
import dk.alfabetacain.adventofcode._2021.Util
import scodec.Decoder
import scodec.Codec
import scodec.Encoder

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      _ <- simpleParseTest()
      _ <- solveFirstTest()
      _ <- solveFirst()
      _ <- solveSecondTest()
      _ <- solveSecond()
    } yield ExitCode.Success
  }

  private def simpleParseTest(): IO[Unit] = {
    IO {

      val vector = hex"38006F45291200".bits
      val parsed = Packet.parse(vector)
      println(s"Parsed: $parsed")
    }
  }

  private def solveFirstTest(): IO[Unit] = {
    IO {
      val bits   = load("16/1_test.txt")
      val parsed = Packet.parse(bits)
      println(s"First test = ${parsed.versionSum}")
    }
  }

  private def solveFirst(): IO[Unit] = {
    IO {
      val bits   = load("16/1.txt")
      val parsed = Packet.parse(bits)
      println(s"First = ${parsed.versionSum}")
    }
  }

  private def solveSecondTest(): IO[Unit] = {
    IO {
      val bits   = hex"9C0141080250320F1802104A08".bits
      val parsed = Packet.parse(bits)
      println(s"Second test = ${parsed.compute()}")
    }
  }

  private def solveSecond(): IO[Unit] = {
    IO {
      val bits   = load("16/1.txt")
      val parsed = Packet.parse(bits)
      println(s"Second = ${parsed.compute()}")
    }
  }

  private def load(path: String): BitVector = {
    val input = Util.loadResource(path)
    BitVector.fromValidHex(input.head)
  }
}
