import scala.annotation.tailrec
import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.io.Source

object Day1 {

  def main(args: Array[String]): Unit = {
    val in =
      Source.fromResource("input/day1.txt").getLines().toList.map(_.toLong)

    val part1 = numOfIncreasingMeasurements(in)

    println(part1)

    val measurements =
      recurse(List.empty, in).map { case (a, b, c) => a + b + c }

    val part2 = numOfIncreasingMeasurements(measurements)

    println(part2)
  }

  def numOfIncreasingMeasurements(in: List[Long]): Int = {
    in.zip(in.tail).foldLeft(0) { case (acc, (l, r)) =>
      if l < r then acc + 1 else acc
    }
  }

  @tailrec
  def recurse(
      acc: List[(Long, Long, Long)],
      in: List[Long]
  ): List[(Long, Long, Long)] = in match {
    case a :: b :: c :: _ => recurse(acc :+ (a, b, c), in.tail)
    case _                => acc
  }

}
