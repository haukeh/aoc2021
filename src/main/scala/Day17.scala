import scala.annotation.tailrec

object Day17 {
  case class Point(x: Int, y: Int)
  case class Velo(x: Int, y: Int)
  case class Area(from: Point, to: Point) {
    def contains(p: Point): Boolean =
      p.x >= from.x && p.x <= to.x && p.y >= from.y && p.y <= to.y
  }

  case class Probe(velo: Velo, pos: Point = Point(0, 0)) {
    def step: Probe = this match {
      case Probe(Velo(vx, vy), Point(x, y)) =>
        val vvx = if (vx < 0) vx + 1 else if (vx > 0) vx - 1 else 0
        Probe(Velo(vvx, vy - 1), Point(x + vx, y + vy))
    }
  }

  @tailrec
  def run(probe: Probe, highPoint: Int, target: Area): Option[Int] = {
    val next = probe.step
    val hp = Math.max(highPoint, next.pos.y)
    if (target.contains(next.pos)) {
      Some(hp)
    } else if (next.pos.y < target.from.y) {
      None
    } else run(next, hp, target)
  }

  def main(args: Array[String]): Unit = {
    val targetArea = readLines("input/17").head match {
      case s"target area: x=${xlow}..${xhigh}, y=${ylow}..${yhigh}" =>
        Area(
          from = Point(xlow.toInt, ylow.toInt),
          to = Point(xhigh.toInt, yhigh.toInt)
        )
    }

    val ps = for (x <- -1000 to 1000; y <- -1000 to 1000) yield run(Probe(Velo(x, y)), 0, targetArea)

    val res = ps.flatten

    val part1 = res.max
    val part2 = res.length

    println(part1)
    println(part2)
  }

}
