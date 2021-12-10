import scala.annotation.tailrec
import scala.collection.mutable

case object Incomplete

object Day10 {

  def main(args: Array[String]): Unit = {
    val input =
      readLines("input/day10.txt").map(_.trim).map(_.toCharArray.toList)

    val parsed = input.map(l => parse(l, List.empty))

    val part1 = parsed.collect { case Right(Some(badChar)) =>
      badChar match {
        case ')' => 3
        case ']' => 57
        case '}' => 1197
        case '>' => 25137
      }
    }.sum

    println(part1)

    val incomplete = input.filter(parse(_, List.empty).isLeft)

    println(incomplete)

  }

  @tailrec
  def parse(
      l: List[Char],
      stack: List[Char]
  ): Either[Incomplete.type, Option[Char]] = {
    l match {
      case Nil => if (stack.isEmpty) Right(None) else Left(Incomplete)
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
              case None                  => Left(Incomplete)
            }
        }
    }
  }
}
