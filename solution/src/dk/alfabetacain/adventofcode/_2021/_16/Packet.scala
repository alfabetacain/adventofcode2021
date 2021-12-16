package dk.alfabetacain.adventofcode._2021._16

import scodec.bits._
import scodec.codecs
import dk.alfabetacain.adventofcode._2021.Util
import scodec.Decoder
import scodec.Codec
import scodec.Encoder

sealed trait Packet {
  def versionSum: Long
  def compute(): Long
}

object Packet {

  private def applyToFirstTwo[A](input: List[Packet])(action: (Long, Long) => A): A = input match {
    case first :: second :: _ => action(first.compute(), second.compute())
    case _                    => ???
  }

  final case class Literal(version: Short, value: Long) extends Packet {
    override def versionSum: Long = version
    override def compute(): Long  = value
  }

  final case class Sum(version: Short, subpackets: List[Packet]) extends Packet {
    override def versionSum: Long = version + subpackets.map(_.versionSum).sum
    override def compute(): Long  = subpackets.map(_.compute()).sum
  }

  final case class Product(version: Short, subpackets: List[Packet]) extends Packet {
    override def versionSum: Long = version + subpackets.map(_.versionSum).sum
    override def compute(): Long  = subpackets.foldLeft[Long](1L)(_ * _.compute())
  }

  final case class Minimum(version: Short, subpackets: List[Packet]) extends Packet {
    override def versionSum: Long = version + subpackets.map(_.versionSum).sum
    override def compute(): Long  = subpackets.map(_.compute()).min
  }

  final case class Maximum(version: Short, subpackets: List[Packet]) extends Packet {
    override def versionSum: Long = version + subpackets.map(_.versionSum).sum
    override def compute(): Long  = subpackets.map(_.compute()).max
  }

  final case class GreaterThan(version: Short, subpackets: List[Packet]) extends Packet {
    override def versionSum: Long = version + subpackets.map(_.versionSum).sum

    override def compute(): Long = applyToFirstTwo(subpackets) { (l, r) => if (l > r) 1 else 0 }
  }

  final case class LessThan(version: Short, subpackets: List[Packet]) extends Packet {
    override def versionSum: Long = version + subpackets.map(_.versionSum).sum

    override def compute(): Long = applyToFirstTwo(subpackets) { (l, r) => if (l < r) 1 else 0 }
  }

  final case class Equal(version: Short, subpackets: List[Packet]) extends Packet {
    override def versionSum: Long = version + subpackets.map(_.versionSum).sum

    override def compute(): Long = applyToFirstTwo(subpackets) { (l, r) => if (l == r) 1 else 0 }
  }

  object Literal {

    private def parseLiteralNumber(): Decoder[Long] = {
      val parse5Bit: Decoder[(Boolean, BitVector)] = for {
        isLast     <- codecs.bool.map(!_)
        numberPart <- codecs.bits(4)
      } yield (isLast, numberPart)

      def doParse(): Decoder[BitVector] = {
        for {
          (isLast, numberPart) <- parse5Bit
          remaining <-
            if (isLast) {
              codecs.bits(0)
            } else {
              doParse()
            }
        } yield numberPart ++ remaining
      }
      doParse.map(_.toLong(signed = false))
    }

    private[Packet] def decoder(version: Short): Decoder[Literal] = {
      for {
        value <- parseLiteralNumber()
      } yield Literal(version, value)
    }
  }

  object Operator {

    private def repeatX[A](decoder: Decoder[A], times: Int): Decoder[List[A]] = {
      if (times == 0) {
        codecs.bits(0).map(_ => List.empty)
      } else {
        for {
          part <- decoder
          rest <- repeatX(decoder, times - 1)
        } yield part :: rest
      }
    }

    private def repeatWhileNonEmpty[A](decoder: Decoder[A]): Decoder[List[A]] = {
      for {
        part      <- decoder
        remaining <- codecs.bitsRemaining
        tail <-
          if (remaining) {
            repeatWhileNonEmpty(decoder)
          } else {
            codecs.bits(0).map(_ => List.empty)
          }
      } yield part :: tail
    }

    private def decoderOnlyCodec[A](decoder: Decoder[A]): Codec[A] = {
      val encoder: Encoder[A] = Encoder[A]((_: A) => throw new IllegalStateException("Encoding not supported!!!"))
      Codec[A](encoder, decoder)
    }

    private[Packet] def decoder(constructor: List[Packet] => Packet): Decoder[Packet] = {
      for {
        isLengthNumberOfPackets <- codecs.bool
        packets <-
          if (isLengthNumberOfPackets) {
            for {
              numberOfPackets <- codecs.bits(11).map(_.toShort(signed = false))
              packets         <- repeatX(Packet.packet, numberOfPackets)
            } yield packets
          } else {
            for {
              numberOfBits <- codecs.bits(15).map(_.toLong(signed = false))
              packets      <- codecs.fixedSizeBits(numberOfBits, decoderOnlyCodec(repeatWhileNonEmpty(packet)))
              isDone       <- codecs.bitsRemaining
            } yield packets
          }
      } yield constructor(packets)
    }
  }

  private val version: Decoder[Short]    = codecs.bits(3).map(_.toShort(signed = false))
  private val packetType: Decoder[Short] = codecs.bits(3).map(_.toShort(signed = false))

  def parse(input: BitVector): Packet = {
    import scodec.codecs.implicits._
    packet.decode(input).require.value
  }

  private def packet: Decoder[Packet] = {
    for {
      version <- version
      typ     <- packetType
      packet <- typ match {
        case 0 => Operator.decoder(Sum(version, _))
        case 1 => Operator.decoder(Product(version, _))
        case 2 => Operator.decoder(Minimum(version, _))
        case 3 => Operator.decoder(Maximum(version, _))
        case 4 => Literal.decoder(version)
        case 5 => Operator.decoder(GreaterThan(version, _))
        case 6 => Operator.decoder(LessThan(version, _))
        case 7 => Operator.decoder(Equal(version, _))
      }
    } yield packet
  }
}
