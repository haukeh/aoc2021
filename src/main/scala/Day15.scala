import scala.collection.mutable

object Day15 {
  def main(args: Array[String]): Unit = {
    val input = readLines("input/day15_ex.txt")

    val matrix = input.map(_.toCharArray.map(_.asDigit).toList)

    val vertices = (for (y <- matrix.indices; x <- matrix.head.indices) yield {
      val neighbours = Seq(
        matrix.lift(y - 1).flatMap(_.lift(x)).map((y - 1, x) -> _), // left
        matrix.lift(y).flatMap(_.lift(x + 1)).map((y, x + 1) -> _), // right
        matrix.lift(y + 1).flatMap(_.lift(x)).map((y + 1, x) -> _), // top
        matrix.lift(y).flatMap(_.lift(x - 1)).map((y, x - 1) -> _) // down
      )

      (y, x) -> (matrix(y)(x), neighbours.flatten)
    }).toMap

    val distances: mutable.Map[(Int, Int), Long] = mutable.Map()
    val previous = mutable.Map()
    val q = mutable.Set.from(vertices.keys)

    for (v <- vertices) {
      distances(v._1) = Long.MaxValue
    }

    distances((0, 0)) = 0

    while q.nonEmpty {
      val u =
    }

    println(vertices)
  }

}
