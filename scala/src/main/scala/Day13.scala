import scala.collection.mutable

object Day13 {

  case class Point(x: Int, y: Int)

  def main(args: Array[String]): Unit = {
    val input = readLines("input/day13.txt")

    val dots = input.takeWhile(_.nonEmpty).map { str =>
      val Array(x, y) = str.split(',')

      (x.toInt, y.toInt)
    }

    val maxX = dots.map(_._1).max
    val maxY = dots.map(_._2).max
    val instructions = input.dropWhile(_.nonEmpty).drop(1).map {
      case s"fold along $axis=$n" => (axis, n.toInt)
    }

    val points =
      dots.map { case (x, y) => (x, y) -> "#" }.toMap

    val grid = mutable.Buffer.fill(maxY + 1, maxX + 1)(" ")

    for (y <- 0 to maxY) {
      for (x <- 0 to maxX) {
        points.get((x, y)).foreach(_ => grid(y)(x) = "#")
      }
    }

    val part1 = instructions
      .take(1)
      .foldLeft(grid)(fold)
      .flatMap(_.filter(_ == "#"))
      .length

    println(part1)

    val part2 = instructions.foldLeft(grid)(fold)

    show(part2)
  }

  def fold(
      grid: mutable.Buffer[mutable.Buffer[String]],
      folding: (String, Int)
  ): mutable.Buffer[mutable.Buffer[String]] = {
    val (axis, num) = folding

    axis match {
      case "y" =>
        val stay = grid.take(num)
        val move = grid.drop(num + 1).reverse

        for (y <- move.indices) {
          for (x <- stay(y).indices) {
            if (move(y)(x) == "#") {
              stay((stay.length - move.length) + y)(x) = "#"
            }
          }
        }

        stay

      case "x" =>
        val stay = grid.map(l => l.take(num))
        val move = grid.map(_.drop(num + 1).reverse)

        for (y <- stay.indices) {
          for (x <- move(y).indices) {
            if (move(y)(x) == "#") {
              stay(y)((stay(y).length - move(y).length) + x) = "#"
            }
          }
        }

        stay
    }
  }

  def show(l: mutable.Buffer[mutable.Buffer[String]]): Unit = println(
    l.map(_.mkString("")).mkString("\n") + "\n"
  )
}
