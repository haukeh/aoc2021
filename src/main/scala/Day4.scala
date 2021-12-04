import Day4.play

import scala.annotation.tailrec
import scala.io.Source

object Day4 {

  def main(args: Array[String]): Unit = {
    val input =
      Source.fromResource("input/day4.txt").getLines().toList

    val numbers =
      input.head.trim.split(",").map(_.toInt).toList
    val boards =
      generateBoards(input.drop(2), List.empty[BingoBoard])

    val (num, firstWinner) = play(numbers, boards)

    val part1 = firstWinner.score(num)

    println(part1)

    val (lastNum, lastWinner) = playUntilLast(numbers, boards)
    val part2 = lastWinner.score(lastNum)

    println(part2)
  }

  @tailrec
  def play(numbers: List[Int], boards: List[BingoBoard]): (Int, BingoBoard) = {
    val number = numbers.head
    val next = boards.map(_.play(number))
    val winner = next.find(_.winningRow().isDefined)
    if (winner.nonEmpty) number -> winner.get
    else play(numbers.drop(1), next)
  }

  @tailrec
  def playUntilLast(
      numbers: List[Int],
      boards: List[BingoBoard]
  ): (Int, BingoBoard) = {
    val number = numbers.head
    val next = boards.map(_.play(number))
    val winners = next.filter(_.winningRow().isDefined)
    winners match {
      case winner :: Nil if next == List(winner) =>
        (number, winner)
      case l @ head :: tail =>
        playUntilLast(numbers.drop(1), next.filterNot(l.contains))
      case Nil =>
        playUntilLast(numbers.drop(1), next)
    }
  }

  @tailrec
  def generateBoards(
      rows: List[String],
      boards: List[BingoBoard]
  ): List[BingoBoard] = {
    if (rows.isEmpty) boards
    else {
      val b = rows
        .takeWhile(!_.isBlank)
        .map(_.trim.split("\\s+").toList.map(_.toInt -> false))
      generateBoards(rows.drop(b.length + 1), boards.appended(BingoBoard(b)))
    }
  }

  case class BingoBoard(rows: List[List[(Int, Boolean)]]) {
    def play(num: Int): BingoBoard =
      this.copy(rows = rows.map(_.map { case current @ (n, _) =>
        if (n == num) n -> true else current
      }))

    def winningRow(): Option[List[(Int, Boolean)]] = {
      val allMarked: List[(Int, Boolean)] => Boolean = _.forall {
        case (_, marked) => marked
      }
      rows.find(allMarked) orElse rows.transpose.find(allMarked)
    }

    def score(num: Int): Int = {
      val sum = rows.flatMap {
        _.collect {
          case (i, marked) if !marked => i
        }
      }.sum

      num * sum
    }

    override def toString: String = rows
      .map(
        _.map { case (i, m) => if (m) s"($i)" else i.toString }.mkString(" ")
      )
      .mkString("\n")
  }
}
