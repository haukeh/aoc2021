object Day8 {

  def main(args: Array[String]): Unit = {
    val l = readLines("input/day8_ex.txt")
      .map {
        _.trim.split('|')
      }

    val pairs = l.map { case Array(l, r) =>
      l.split(' ').map(_.trim).toList -> r.split(' ').map(_.trim).toList
    }

    val part1 =
      pairs.flatMap(_._2).count(n => Set(2, 3, 4, 7).contains(n.length))

    println(part1)

    deduce(pairs.take(1).flatMap(_._1))

    def deduce(l: List[String]): Map[String, Int] = {
      val easyValues = l.sortBy(_.length).foldLeft(Map.empty[Int, Set[Char]]) { case (m, elem) =>
        elem match {
          case s if s.length == 2 => m.updated(1, s.toCharArray.toSet)
          case s if s.length == 3 => m.updated(7, s.toCharArray.toSet)
          case s if s.length == 4 => m.updated(4, s.toCharArray.toSet)
          case s if s.length == 7 => m.updated(8, s.toCharArray.toSet)
          case _ => m
        }
      }

      println(easyValues(1))
      println(easyValues(7))
      println(easyValues(4))
      println(easyValues(8))

      val topRow = easyValues(7) diff easyValues(1)

      println(topRow)

      val midAndLeft = easyValues(7) diff easyValues(4)

      val bottomRow = l.drop(4).map(_.toCharArray.toSet).find { i =>
        (easyValues(7) union easyValues(4)).diff(i).size == 1
      }.head

      val midRow = l.drop(4).map(_.toCharArray.toSet).find { i =>
        (easyValues(7) union bottomRow).diff(i).size == 1
      }

      println(topRow)
      println(midAndLeft)
      println(midRow)
      println(bottomRow)

      Map.empty
    }

  }
}
