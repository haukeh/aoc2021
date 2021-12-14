import org.graalvm.collections.Pair

import scala.collection.mutable

object Day14 {
  def main(args: Array[String]): Unit = {
    val input = readLines("input/day14.txt")

    val template = input.take(1).head

    val rules = input
      .drop(2)
      .map { case s"$pair -> $add" =>
        val Array(a, b) = pair.toCharArray
        (a, b) -> add.toCharArray.head
      }
      .toMap

    val p = template.toCharArray
      .zip(template.toCharArray.drop(1))
      .groupBy(identity)
      .view
      .mapValues(_.length.toLong)

    val c =
      template.toCharArray.groupBy(identity).view.mapValues(_.length.toLong)

    val pairs = mutable.Map.from(p).withDefaultValue(0L)
    val chars = mutable.Map.from(c).withDefaultValue(0L)

    for (_ <- 0 until 40) {
      pairs.toList.foreach { case (pair, cnt) =>
        val add = rules(pair)
        chars(add) += cnt
        pairs(pair) -= cnt
        pairs((pair._1, add)) += cnt
        pairs((add, pair._2)) += cnt
      }
    }

    val (_, max) = chars.maxBy(_._2)
    val (_, min) = chars.minBy(_._2)

    println(max - min)
  }
}
