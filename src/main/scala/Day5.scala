import scala.collection.mutable.ArrayBuffer

case class Point(x: Int, y: Int)
case class Line(a: Point, b: Point)

object Day5 {
  private val Pattern = "(\\d+),(\\d+) -> (\\d+),(\\d+)".r

  @main def main(): Unit = {
    val input = readLines("input/day5.txt")

    val lines = input.map { case Pattern(x1, y1, x2, y2) =>
      val p1 = Point(x1.toInt, y1.toInt)
      val p2 = Point(x2.toInt, y2.toInt)
      Line(p1, p2)
    }

    val part1 = calculateOverlaps(lines, noDiagonals = true)
    val part2 = calculateOverlaps(lines)

    println(part1)
    println(part2)
  }

  def calculateOverlaps(lines: Seq[Line], noDiagonals: Boolean = false): Int = {
    val max = lines.flatMap(l => Seq(l.a.x, l.b.x, l.a.y, l.b.y)).max

    val system = ArrayBuffer.fill(max + 1, max + 1)(0)

    lines.foreach { case l @ Line(p1, p2) =>
      val xStep = if (p1.x >= p2.x) -1 else 1
      val yStep = if (p1.y >= p2.y) -1 else 1

      if (p1.x == p2.x || p1.y == p2.y) {
        for (y <- p1.y to p2.y by yStep; x <- p1.x to p2.x by xStep) {
          system(y)(x) += 1
        }
      } else if (!noDiagonals) {
        for ((y, x) <- (p1.y to p2.y by yStep) zip (p1.x to p2.x by xStep)) {
          system(y)(x) += 1
        }
      }
    }

    system.foldLeft(0) { (acc, row) =>
      acc + row.count(_ > 1)
    }
  }
}
