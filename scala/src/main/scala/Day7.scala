object Day7 {

  def main(args: Array[String]): Unit = {
    val crabs =
      readLines("input/day7.txt").head.trim.split(",").map(_.toInt).toList

    val linearDistances = (0 to crabs.max).map { pos =>
      crabs.map(crabPos => Math.abs(pos - crabPos)).sum
    }

    val part1 = linearDistances.min

    println(part1)

    val triangularDistances = (0 to crabs.max).map { pos =>
      crabs.map { crabPos =>
        val n = Math.abs(pos - crabPos)
        (n * (n + 1)) / 2
      }.sum
    }

    val part2 = triangularDistances.min

    println(part2)
  }
}
