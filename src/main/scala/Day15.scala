import scala.collection.mutable

case class State(cost: Int, position: (Int, Int))

object State {
  implicit val ord: Ordering[State] = Ordering.by(-_.cost)
}

object Day15 {

  def main(args: Array[String]): Unit = {
    val input = readLines("input/day15_ex.txt")

    val matrix = input.map(_.toCharArray.map(_.asDigit).toList)

    val target = (matrix.indices.last, matrix.head.indices.last)

    val vertices = (for (y <- matrix.indices; x <- matrix.head.indices) yield {
      val neighbours = Seq(
        matrix.lift(y - 1).flatMap(_.lift(x)).map((y - 1, x) -> _), // left
        matrix.lift(y).flatMap(_.lift(x + 1)).map((y, x + 1) -> _), // right
        matrix.lift(y + 1).flatMap(_.lift(x)).map((y + 1, x) -> _), // top
        matrix.lift(y).flatMap(_.lift(x - 1)).map((y, x - 1) -> _) // down
      )
      (y, x) -> (matrix(y)(x), neighbours.flatten)
    }).toMap

    val distances: mutable.Map[(Int, Int), Long] = mutable.Map().withDefaultValue(Long.MaxValue)
    val queue = mutable.PriorityQueue.empty[State]

    distances((0, 0)) = 0
    queue.enqueue(State(0, (0, 0)))

    var goal: Option[Int] = None
    while (queue.nonEmpty && goal.isEmpty) {
      val State(cost, position) = queue.dequeue()

      if (position == target) goal = Some(cost)

      if (cost <= distances(position)) {
        for (neighbour <- vertices(position)._2) {
          val next = State(cost + neighbour._2, neighbour._1)

          if (next.cost < distances(next.position)) {
            queue.enqueue(next)
            distances(next.position) = next.cost
          }
        }
      }
    }

    println(goal)
  }

}
