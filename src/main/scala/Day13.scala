import scala.collection.mutable

object Day13 {

  case class Point(x: Int, y: Int)

  def main(args: Array[String]): Unit = {
    val input = readLines("input/day13_ex.txt")

    val dots = input.takeWhile(_.nonEmpty).map { str =>
      val Array(x, y) = str.split(',')

      Point(x.toInt, y.toInt)
    }

    val foldings = input.dropWhile(_.nonEmpty).drop(1).map {
      case s"fold along $axis=$n" => (axis, n.toInt)
    }

    val grid =
      mutable.Map
        .from(
          dots.map { case Point(x, y) => (x, y) }
        )
        .withDefaultValue(10)

    println(dots)
    println(foldings)
  }
}
