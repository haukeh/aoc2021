import cats.data.State
import cats.implicits.*
import cats.syntax.*
import cats.syntax.monad.catsSyntaxMonad

import scala.annotation.tailrec

object Day16 {

  case class Header(version: Int, typeID: Int)

  sealed trait Packet {
    def header: Header

    def sumVersions: Int = this match {
      case Literal(Header(version, _), _) => version
      case Operator(Header(version, _), packets) =>
        version + packets.map(_.sumVersions).sum
    }

    def expr: Long = this match {
      case Literal(_, value) => value
      case Operator(header, packets) => header match {
        case Header(_, 0) => packets.map(_.expr).sum
        case Header(_, 1) => packets.map(_.expr).product
        case Header(_, 2) => packets.map(_.expr).min
        case Header(_, 3) => packets.map(_.expr).max
        case Header(_, 5) => if (packets.head.expr > packets(1).expr) 1 else 0
        case Header(_, 6) => if (packets.head.expr < packets(1).expr) 1 else 0
        case Header(_, 7) => if (packets.head.expr == packets(1).expr) 1 else 0
      }
    }
  }

  case class Literal(header: Header, value: Long) extends Packet
  case class Operator(header: Header, packets: Seq[Packet]) extends Packet

  def main(args: Array[String]): Unit = {
    val input = readLines("input/day16.txt").head.trim

    val bits = BigInt(input, 16).toString(2)

    val res: Packet = parsePacket.runA(bits).value

    println(res)

    val part1 = res.sumVersions
    val part2 = res.expr
    println(part1)
    println(part2)
  }

  def parseBinaryDigits(num: Int): State[String, Int] =
    State(str => (str.drop(num), Integer.parseInt(str.take(num), 2)))

  def parseVersion: State[String, Int] = parseBinaryDigits(3)

  def parseTypeId: State[String, Int] = parseBinaryDigits(3)

  def takeStr(num: Int): State[String, String] =
    State(str => (str.drop(num), str.take(num)))

  def parseHeader: State[String, Header] = for {
    version <- parseVersion
    typeID <- parseTypeId
  } yield Header(version, typeID)

  def parseLiteralValue(digits: Seq[String]): State[String, Long] =
    for {
      lastGroup <- parseBinaryDigits(1).map(_ == 0)
      rawDigit <- takeStr(4)
      next = digits :+ rawDigit
      res <-
        if (lastGroup)
          State.pure(java.lang.Long.parseLong(next.mkString, 2))
        else parseLiteralValue(next)
    } yield res

  def parseLengthTypeId: State[String, Int] = parseBinaryDigits(1)

  def parseOperatorPacket(header: Header): State[String, Packet] =
    parseLengthTypeId.flatMap {
      case 1 =>
        parseBinaryDigits(11).flatMap(num =>
          parsePacket
            .replicateA(num)
            .map(subPackets => Operator(header, subPackets))
        )
      case 0 =>
        parseBinaryDigits(15)
          .flatMap(parsePacketsUntil)
          .map(sp => Operator(header, sp))
    }

  import cats.syntax.MonadOps.*

  def parsePacketsUntil(len: Int): State[String, Seq[Packet]] = for {
    initialLen <- State.inspect[String, Int](_.length)
    packets <- parsePacket.whileM[Vector](
      State
        .inspect[String, Int](_.length)
        .map(newLen => initialLen - newLen < len)
    )
  } yield packets

  def parsePacket: State[String, Packet] =
    parseHeader.flatMap {
      case h @ Header(_, 4) =>
        parseLiteralValue(Seq.empty[String]).map(Literal(h, _))
      case h @ Header(_, _) => parseOperatorPacket(h)
    }
}
