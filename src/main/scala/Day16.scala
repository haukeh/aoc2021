import scala.annotation.tailrec
import scala.collection.BitSet

object Day16 {

  def main(args: Array[String]): Unit = {
    val input = readLines("input/day16_ex.txt").head.trim

    val bits = BigInt(input, 16).toString(2)

    val version = Integer.parseInt(bits.take(3), 2)
    val t = Integer.parseInt(bits.take(3), 2)

    val num = parseLiteralNumber(bits.drop(6))

    println(num)
  }

  def parseLiteralNumber(str: String): Long = {
    @tailrec
    def take(str: String, acc: Seq[String]): Seq[String] = {
      if (str.length < 5) {
        acc
      } else if (str.startsWith("1")) {
        take(str.drop(5), acc.appended(str.slice(1, 5)))
      } else {
        acc.appended(str.slice(1, 5))
      }
    }

    val nums = take(str, Seq.empty).mkString

    Integer.parseInt(nums, 2)
  }
}
