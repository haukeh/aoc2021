import scala.io.Source

object Day2 {

  def main(args: Array[String]): Unit = {
    val input = Source
      .fromResource("input/day2.txt")
      .getLines()
      .filterNot(_.isBlank)
      .toList
      .map(_.trim.split(' '))

    println(p1(input))
    println(p2(input))

  }

  def p1(commands: Seq[Array[String]]): Int = {
    val (p, d) = commands.foldLeft((0, 0)) {
      case ((pos, depth), Array(action, amount)) if action == "forward" =>
        (pos + amount.toInt, depth)
      case ((pos, depth), Array(action, amount)) if action == "up" =>
        (pos, depth - amount.toInt)
      case ((pos, depth), Array(action, amount)) if action == "down" =>
        (pos, depth + amount.toInt)
    }

    p * d
  }

  def p2(commands: Seq[Array[String]]): Long =
    val (p, d, _) = commands.foldLeft((0L, 0L, 0L)) {
      case ((pos, depth, aim), Array(action, amount)) if action == "down" =>
        (pos, depth, aim + amount.toLong)
      case ((pos, depth, aim), Array(action, amount)) if action == "up" =>
        (pos, depth, aim - amount.toLong)
      case ((pos, depth, aim), Array(action, amount)) if action == "forward" =>
        (pos + amount.toLong, depth + (aim * amount.toLong), aim)
    }

    p * d
}
