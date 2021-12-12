import scala.collection.mutable

object Day12 {

  def main(args: Array[String]): Unit = {
    val c = mutable.Map[String, mutable.Set[String]]()

    val input = readLines("input/day12.txt")
      .flatMap { l =>
        l.trim.split('-') match {
          case Array("start", to) => Seq("start" -> to)
          case Array(to, "start") => Seq("start" -> to)
          case Array(from, "end") => Seq(from -> "end")
          case Array(from, to)    => Seq(from -> to, to -> from)
        }
      }
      .foldLeft(Map.empty[String, Set[String]]) { case (m, (k, v)) =>
        m.updatedWith(k) {
          case Some(set) => Some(set + v)
          case None      => Some(Set(v))
        }
      }
      .withDefaultValue(Set.empty)

    val s = traverse("start", input, Seq.empty)

    println(s.size)
  }

  def traverse(
      p: String,
      m: Map[String, Set[String]],
      l: Seq[String],
      twice: Option[String] = None
  ): Iterable[Seq[String]] =
    if (p == "end")
      Seq(l)
    else
      m(p)
        .collect {
          case s if isUpperCase(s) || !l.contains(s) => s -> twice
          case s if twice.exists(t => t == s && l.count(_ == t) < 2) =>
            s -> twice
          case s if l.contains(s) && twice.isEmpty => s -> Some(s)
        }
        .flatMap { case (str, double) =>
          traverse(str, m, l :+ str, double)
        }

  private def isUpperCase(s: String) = s.toCharArray.forall(_.isUpper)
}
