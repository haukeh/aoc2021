import scala.collection.mutable

object Day11 {

  def main(args: Array[String]): Unit = {
    val input = readLines("input/day11.txt")
      .map(_.toCharArray)
      .zipWithIndex
      .flatMap { case (row, y) =>
        row.zipWithIndex.map { case (c, x) => (y, x) -> c.asDigit }
      }

    val m1: mutable.Map[(Int, Int), Int] = mutable.Map(input: _*)

    val part1 = (for (_ <- 1 to 100) yield step(m1).size).sum
    println(part1)

    var rounds = 0
    var allFlashed = false
    val m2: mutable.Map[(Int, Int), Int] = mutable.Map(input: _*)

    while (!allFlashed) {
      val next = step(m2)
      if (next == m2.keySet) allFlashed = true
      rounds += 1
    }

    println(rounds)
  }

  private def step(m: mutable.Map[(Int, Int), Int]): mutable.Set[(Int, Int)] = {
    val flashers = mutable.Set[(Int, Int)]()

    for (x <- 0 until 10; y <- 0 until 10) {
      m((x, y)) = m((x, y)) + 1
    }

    for (x <- 0 until 10; y <- 0 until 10) {
      val p = (x, y)
      if (m(p) > 9) {
        flash(flashers, p)
      }
    }

    flashers.foreach {
      m(_) = 0
    }

    def flash(flashers: mutable.Set[(Int, Int)], p: (Int, Int)): Unit = {
      val orientations = Seq(
        (0, 1),
        (1, 0),
        (-1, 0),
        (0, -1),
        (1, 1),
        (-1, 1),
        (1, -1),
        (-1, -1)
      )

      m.get(p).foreach { num =>
        if (!flashers.contains(p)) {
          flashers.add(p)

          for ((dx, dy) <- orientations) {
            val pp = (p._1 + dx, p._2 + dy)
            m.get(pp).foreach { _ =>
              if (!flashers.contains(pp)) {
                m(pp) += 1
                if (m(pp) > 9) flash(flashers, pp)
              }
            }
          }
        }
      }
    }

    flashers
  }

  def show(m: mutable.Map[(Int, Int), Int]): Unit = {
    val x =
      for (x <- 0 until 10)
        yield for (y <- 0 until 10) yield m((x, y))

    println(x.map(_.mkString(" ")).mkString("\n"))
    println("\n")
  }
}
