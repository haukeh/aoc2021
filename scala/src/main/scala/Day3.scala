import scala.annotation.tailrec
import scala.io.Source

object Day3 {

  def main(args: Array[String]): Unit = {
    val in =
      Source.fromResource("input/day3.txt").getLines().toList

    val positions = in.transpose

    val gammaRate =
      Integer.parseUnsignedInt(positions.map(mostCommon).mkString, 2)

    val epsilonRate =
      Integer.parseUnsignedInt(positions.map(leastCommon).mkString, 2)

    val part1 = gammaRate * epsilonRate

    println(part1)

    val oxygen = calculate(mostCommon)(in, 0, positions)
    val co2 = calculate(leastCommon)(in, 0, positions)

    val part2 =
      Integer.parseUnsignedInt(oxygen, 2) * Integer.parseUnsignedInt(co2, 2)

    println(part2)
  }

  @tailrec
  def calculate(op: List[Char] => Char)(
      remaining: List[String],
      pos: Int,
      positions: List[List[Char]]
  ): String = {
    remaining match {
      case last :: Nil =>
        last
      case _ =>
        val wantedChar = op(positions(pos))
        val next = remaining.filter(_.charAt(pos) == wantedChar)
        calculate(op)(
          next,
          pos + 1,
          next.transpose
        )
    }
  }

  def mostCommon(chars: List[Char]): Char =
    chars.partition(_ == '1') match {
      case (ones, zeroes) =>
        if (ones.size > zeroes.size || ones.size == zeroes.size) '1' else '0'
    }

  def leastCommon(chars: List[Char]): Char =
    chars.partition(_ == '0') match {
      case (zeroes, ones) =>
        if (zeroes.size < ones.size || ones.size == zeroes.size) '0' else '1'
    }
}
