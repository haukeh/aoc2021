import scala.annotation.tailrec
import scala.collection.mutable

object Day10 {

  def main(args: Array[String]): Unit = {
    val input =
      readLines("input/day10.txt").map(_.trim).map(_.toCharArray.toList)

    val parsed = input.map(parse(_, List.empty))

    val part1 = parsed.collect {
      case Right(Some(')')) => 3
      case Right(Some(']')) => 57
      case Right(Some('}')) => 1197
      case Right(Some('>')) => 25137
    }.sum

    println(part1)

    val completionScores = input
      .map(parse(_, List.empty))
      .collect { case Left(chars) => chars }
      .map {
        _.map {
          case '(' => 1L
          case '[' => 2L
          case '{' => 3L
          case '<' => 4L
        }.foldLeft(0L) { (score, next) =>
          5L * score + next
        }
      }
      .sorted

    val part2 = completionScores((completionScores.length - 1) / 2)

    println(part2)
  }

  @tailrec
  def parse(
      l: List[Char],
      stack: List[Char]
  ): Either[List[Char], Option[Char]] = {
    l match {
      case Nil => if (stack.isEmpty) Right(None) else Left(stack)
      case head :: tail =>
        head match {
          case '{' => parse(tail, stack.prepended('{'))
          case '[' => parse(tail, stack.prepended('['))
          case '<' => parse(tail, stack.prepended('<'))
          case '(' => parse(tail, stack.prepended('('))
          case c @ ('}' | ']' | '>' | ')') =>
            stack.headOption match {
              case Some('{') if c == '}' => parse(tail, stack.tail)
              case Some('[') if c == ']' => parse(tail, stack.tail)
              case Some('<') if c == '>' => parse(tail, stack.tail)
              case Some('(') if c == ')' => parse(tail, stack.tail)
              case Some(other)           => Right(Some(head))
              case None                  => Left(stack)
            }
        }
    }
  }
}
