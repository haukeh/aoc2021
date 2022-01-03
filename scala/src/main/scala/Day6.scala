import scala.collection.mutable

object Day6 {

  @main def run(): Unit = {
    val fish =
      readLines("input/day6.txt").head.trim.split(",").map(_.toInt).toList

    val part1 = simulate(fish, days = 80)
    val part2 = simulate(fish, days = 256)

    println(part1)
    println(part2)
  }

  def simulate(input: Seq[Int], days: Int): Long = {
    val state = mutable.Map.empty[Int, Long].withDefaultValue(0)

    input.foreach { f =>
      state(f) += 1
    }

    for(_ <- 1 to days) {
      val zeroes = state(0)
      state(0) = 0
      for (i <- 1 to 8) {
        val cur = state(i)
        state(i-1) += cur
        state(i) = 0
      }
      state(6) += zeroes
      state(8) += zeroes
    }

    state.values.sum
  }

}
