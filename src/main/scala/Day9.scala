object Day9 {
  def main(args: Array[String]): Unit = {
    val input = readLines("input/day9.txt").map {
      _.trim.toCharArray.map(_.asDigit).toList
    }

    val grid = (for (y <- input.indices; x <- input.head.indices)
      yield (y, x) -> input(y)(x)).toMap.withDefaultValue(10)

    val lowPoints = grid.flatMap { case ((y, x), i) =>
      val top = grid((y + 1, x))
      val right = grid((y, x + 1))
      val bottom = grid((y - 1, x))
      val left = grid((y, x - 1))

      if (i < top && i < right && i < bottom && i < left) Some(i) else None
    }

    val part1 = lowPoints.map(_ + 1).sum

    println(part1)

  }
}
