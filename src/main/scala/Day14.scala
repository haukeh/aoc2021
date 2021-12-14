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

    val res = expand(template.toCharArray, rules, 2)

    println(res)
  }

//  @tailrec
  def expand(template: Array[Char], rules: Map[(Char,Char), Char], iterations: Int, round: Int = 1): Unit = {
    if (round == iterations) template.mkString("") else {
      val pairs = template.sliding(2).take(3)

      println(pairs.map(_.mkString("Array(", ", ", ")")).mkString(""))
//      val c = pairs.flatMap { p =>
//        rules.get(p).map(i => Array[Char](p._1, i, p._2)).getOrElse(Array[Char](p._1, p._2))
//      }
//
//      expand(c, rules, iterations, round + 1)
    }


  }

}
