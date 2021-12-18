import cats.data.State

import scala.annotation.tailrec
import scala.collection.BitSet

object Day16 {

  case class Header(version: Int, typeID: Int)

  def main(args: Array[String]): Unit = {
    val input = readLines("input/day16_ex.txt").head.trim

    val bits = BigInt(input, 16).toString(2)

    val parseHeader: State[String, Header] = for {
      version <- parseVersion
      typeID <- parseTypeId
    } yield Header(version, typeID)

    parseHeader.flatMap {

    }


  }

  val parseVersion: State[String, Int] = parseDigits(3)

  val parseTypeId: State[String, Int] = parseDigits(3)

  def parseDigits(num: Int): State[String, Int] = State(str => (str.drop(num), Integer.parseInt(str.take(num), 2)))

  val parseLiteralValue: State[String, Int] = State { str =>
    @tailrec
    def take(str: String, acc: Seq[String]): (Seq[String], String) = {
      if (str.length < 5) {
        (acc, str)
      } else if (str.startsWith("1")) {
        take(str.drop(5), acc.appended(str.slice(1, 5)))
      } else {
        (acc.appended(str.slice(1, 5)), str)
      }
    }

    val (nums, tail) = take(str, Seq.empty)

    (tail, Integer.parseInt(nums.mkString, 2))
  }
}
