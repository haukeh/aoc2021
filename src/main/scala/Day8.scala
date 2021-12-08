import scala.collection.mutable

object Day8 {

  def main(args: Array[String]): Unit = {
    val l = readLines("input/day8.txt")
      .map {
        _.trim.split('|')
      }

    val pairs = l.map { case Array(l, r) =>
      l.trim.split(' ').toList -> r.trim.split(' ').toList
    }

    val part1 =
      pairs.flatMap(_._2).count(n => Set(2, 3, 4, 7).contains(n.length))

    println(part1)

    val res = pairs.map { case (input, output) =>
      val dict = deduce((input ++ output).map(_.toCharArray.toSet).distinct)
      val digits = output.map { str =>
        dict(str.toCharArray.toSet)
      }.mkString

      Integer.parseInt(digits)
    }

    println(res.sum)
  }

  def deduce(l: List[Set[Char]]): mutable.Map[Set[Char], Int] = {
    val easyValues =
      l.sortBy(_.size).foldLeft(Map.empty[Int, Set[Char]]) { case (m, elem) =>
        elem match {
          case s if s.size == 2 => m.updated(1, s)
          case s if s.size == 3 => m.updated(7, s)
          case s if s.size == 4 => m.updated(4, s)
          case s if s.size == 7 => m.updated(8, s)
          case _                => m
        }
      }

    val one = easyValues(1)
    val seven = easyValues(7)
    val four = easyValues(4)
    val eight = easyValues(8)

    val topRow = seven diff one

    val m = mutable.Map(
      one -> 1,
      four -> 4,
      seven -> 7,
      eight -> 8
    )

    val bottomLeftBottom = eight diff (four | seven)

    val nineS = l
      .filterNot(m.contains)
      .filter { i =>
        i.size == 6 && four.intersect(i) == four
      }

    val nine = nineS.head

    m.put(nine, 9)

    val bottomLeft = eight diff nine

    val sixAndZero = l.filterNot(m.contains).filter { i =>
      i.size == 6
    }

    val (sixS, zeroS) = sixAndZero.partition(i => !(one.intersect(i) == one))

    val six = sixS.head
    val zero = zeroS.head

    m.put(six, 6)
    m.put(zero, 0)

    val fiveS = l.filterNot(m.contains).filter { i =>
      bottomLeft.intersect(i).isEmpty && one.intersect(i) != one
    }

    val five = fiveS.head

    m.put(five, 5)

    val (two, three) = l
      .filterNot(m.contains)
      .partition(item => item.contains(bottomLeft.head))

    m.put(two.head, 2)
    m.put(three.head, 3)

    m
  }
}
