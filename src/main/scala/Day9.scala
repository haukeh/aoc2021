import scala.annotation.tailrec
import scala.collection.mutable

object Day9 {
  def main(args: Array[String]): Unit = {
    val input = readLines("input/day9.txt").map {
      _.trim.toCharArray.map(_.asDigit).toList
    }

    val grid =
      mutable.Map
        .from(
          (for (y <- input.indices; x <- input.head.indices)
            yield (y, x) -> input(y)(x)).toMap
        )
        .withDefaultValue(10)

    val lowPoints = grid.flatMap { case ((y, x), i) =>
      val top = grid((y + 1, x))
      val right = grid((y, x + 1))
      val bottom = grid((y - 1, x))
      val left = grid((y, x - 1))

      if (i < top && i < right && i < bottom && i < left) Some((y, x), i)
      else None
    }

    val part1 = lowPoints.map(_._2 + 1).sum

    println(part1)

    def findBasin(
        start: (Int, Int)
    ): Seq[Int] = {
      val (y, x) = start
      val p = grid(start)
      grid(start) = 10

      if (p > 8)
        Seq()
      else
        Seq(p)
          ++ findBasin((y + 1, x))
          ++ findBasin((y, x + 1))
          ++ findBasin((y - 1, x))
          ++ findBasin((y, x - 1))
    }

    val basinSizes = lowPoints.keys
      .map(findBasin)
      .toList
      .map(_.length)
      .sorted(Ordering[Int].reverse)
      .take(3)

    val part2 = basinSizes.product

    println(part2)
  }

}
