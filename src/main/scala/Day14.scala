import scala.annotation.tailrec

object Day14 {
  def main(args: Array[String]): Unit = {
    val input = readLines("input/day14_ex.txt")

    val template = input.take(1).head

    val rules = input
      .drop(2)
      .map { case s"$pair -> $add" =>
        val Array(a, b) = pair.toCharArray
        (a, b) -> add.toCharArray.head
      }
      .toMap

    val pairs = template.zip(template.drop(1))

    @tailrec
    def loop(input: Array[Char], limit: Int, i: Int = 0): String = {
      if (i == limit) input.mkString
      else {
        val res = expand(input, rules, 0)
        loop(res.toCharArray, limit, i + 1)
      }
    }

    val res = loop(input.head.toCharArray, 40)

    val x = res.toCharArray.groupBy(identity).view.mapValues(_.length)
    val max = x.maxBy(_._2)
    val min = x.minBy(_._2)

    println(max._2 - min._2)
  }

  @tailrec
  def expand(
      template: Array[Char],
      rules: Map[(Char, Char), Char],
      pos: Int
  ): String = {
    if (pos == template.length - 1) {
      template.mkString
    } else {
      val Array(a, b) = template.slice(pos, pos + 2)
      val nextT = rules.get((a, b)).fold(template) { newChar =>
        val (head, tail) = template.splitAt(pos + 1)
        val next = head ++ Array(newChar) ++ tail
        next
      }
      expand(nextT, rules, pos + 2)
    }
  }

}
