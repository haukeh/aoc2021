import scala.annotation.tailrec
import scala.collection.mutable

object Day20 {

  def main(args: Array[String]): Unit = {
    val input = readLines("input/20")

    val algo = input.head.toCharArray

    val rows = input
      .drop(2)
      .map(_.toCharArray.map(c => if (c == '#') 1 else 0).toList)

    val grid = mutable.Map
      .from(
        (for (y <- rows.indices; x <- rows.head.indices)
          yield (y, x) -> rows(y)(x)).toMap
      )

    val p1 = enhance(algo, grid, 0, loops = 50)

    val num = p1.map {
      case (tuple, i) => i
    }.sum

    println(num)
  }

  @tailrec
  def enhance(
      algo: Seq[Char],
      grid: mutable.Map[(Int, Int), Int],
      currentDefault: Int,
      loops: Int,
      cnt: Int = 0
  ): mutable.Map[(Int, Int), Int] = {

    def getPos(p: Int): Int = if algo(p) == '#' then 1 else 0

    val positions = Seq(
      (-1, -1),
      (-1, 0),
      (-1, 1),
      (0, -1),
      (0, 0),
      (0, 1),
      (1, -1),
      (1, 0),
      (1, 1)
    )

    val minY = grid.keys.map(_._1).min
    val minX = grid.keys.map(_._2).min
    val maxY = grid.keys.map(_._1).max
    val maxX = grid.keys.map(_._2).max

    val next = mutable.Map[(Int, Int), Int]()

    for (y <- minY - 2 until maxY + 2) {
      for (x <- minX - 2 until maxX + 2) {
        val bin = positions.map { case (dy, dx) =>
          grid.getOrElse((dy + y, dx + x), currentDefault)
        }.mkString

        val dec = Integer.parseInt(bin, 2)

        next((y, x)) = getPos(dec)
      }
    }

    val nextDefault =
      if (cnt % 2 == 0) 1 else 0

    if (cnt + 1 == loops) next
    else enhance(algo, next, nextDefault, loops, cnt + 1)
  }

  def show(l: mutable.Buffer[mutable.Buffer[Int]]): Unit = println(
    l.map(_.map(s => if (s == 1) "#" else ".").mkString(""))
      .mkString("\n") + "\n"
  )
}
