import scala.collection.mutable

object Day8 {

  def main(args: Array[String]): Unit = {
    val l = readLines("input/day8.txt").map(_.split('|'))

    val io = l.map { case Array(l, r) =>
      l.trim.split(' ').toList -> r.trim.split(' ').toList
    }

    val part1 =
      io.flatMap(_._2).count(n => Set(2, 3, 4, 7).contains(n.length))

    println(part1)

    val outputNumbers = io.map { case (input, output) =>
      val mapping =
        deduceDigitMapping((input ++ output).map(_.toCharArray.toSet).distinct)

      val digits = output.map(_.toCharArray.toSet).map(mapping).mkString

      Integer.parseInt(digits)
    }

    val part2 = outputNumbers.sum

    println(part2)
  }

  def deduceDigitMapping(l: List[Set[Char]]): mutable.Map[Set[Char], Int] = {
    val one :: seven :: four :: tail = l.sortBy(_.size)
    val eight = tail.last

    val m = mutable.Map(
      one -> 1,
      four -> 4,
      seven -> 7,
      eight -> 8
    )

    val nine = l
      .filterNot(m.contains)
      .find { i =>
        i.size == 6 && four.intersect(i) == four
      }
      .get

    m.put(nine, 9)

    val (six :: Nil, zero :: Nil) = l
      .filterNot(i => m.contains(i) || i.size != 6)
      .partition(i => !(one.intersect(i) == one))

    m.put(six, 6)
    m.put(zero, 0)

    val five = l
      .filterNot(m.contains)
      .find { i =>
        (eight diff nine).intersect(i).isEmpty && one.intersect(i) != one
      }
      .get

    m.put(five, 5)

    val (two :: Nil, three :: Nil) = l
      .filterNot(m.contains)
      .partition(item => item.contains((eight diff nine).head))

    m.put(two, 2)
    m.put(three, 3)

    m
  }
}
