import scala.collection.mutable

case class State(cost: Int, position: (Int, Int))

object State {
  implicit val ord: Ordering[State] = Ordering.by(-_.cost)
}

object Day15 {

  def main(args: Array[String]): Unit = {
    val input = readLines("input/day15.txt")

    val tile = input.map(_.toCharArray.map(_.asDigit).toList)
    val top = tile.map(fillRow)

    val cave = (1 to 4).foldLeft(Seq(top)) { (ll, _) =>
      val next = ll.last.map {
        _.map(old => if (old + 1 > 9) 1 else old + 1)
      }
      ll :+ next
    }.flatten

    val target = (cave.indices.last, cave.head.indices.last)

    val vertices = (for (y <- cave.indices; x <- cave.head.indices) yield {
      val neighbours = Seq(
        cave.lift(y - 1).flatMap(_.lift(x)).map((y - 1, x) -> _), // left
        cave.lift(y).flatMap(_.lift(x + 1)).map((y, x + 1) -> _), // right
        cave.lift(y + 1).flatMap(_.lift(x)).map((y + 1, x) -> _), // top
        cave.lift(y).flatMap(_.lift(x - 1)).map((y, x - 1) -> _) // down
      )
      (y, x) -> (cave(y)(x), neighbours.flatten)
    }).toMap

    val distances: mutable.Map[(Int, Int), Long] =
      mutable.Map().withDefaultValue(Long.MaxValue)
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

  def fillRow(t: List[Int]): Seq[Int] = {
    (1 until 5)
      .foldLeft(Seq(t)) { (ts, _) =>
        val next = ts.last.map { old => if (old + 1 > 9) 1 else old + 1 }
        ts :+ next
      }
      .flatten
  }

  def show(cave: Seq[Seq[Int]]): Unit = {
    println(cave.map(_.mkString).mkString("\n"))
  }
}
