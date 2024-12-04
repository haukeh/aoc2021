import cats.data.State

class DeterministicDie {
  var rolls: Int = 0
  var i: Int = 0

  def roll(): Int = {
    rolls += 1
    if (i == 100) i = 1 else i += 1
    i
  }
}

class DeterministicGame(p1Start: Int, p2Start: Int) {
  private var p1score = 0
  private var p1pos = p1Start
  private var p2score = 0
  private var p2pos = p2Start
  private val die = new DeterministicDie

  def run(): (Int, Int, Int) = {
    p1pos = (die.roll() + die.roll() + die.roll() + p1pos - 1) % 10 + 1
    p1score += p1pos
    if (p1score < 21) {
      p2pos = (die.roll() + die.roll() + die.roll() + p2pos - 1) % 10 + 1
      p2score += p2pos
    }
    (p1score, p2score, die.rolls)
  }

}

case class State(
    p1pos: Int,
    p2pos: Int,
    p1Score: Int,
    p2Score: Int,
    p1: Boolean
) {
  def step(roll: Int): State = {
    val nextp1pos = if p1 then (p1pos + roll - 1) % 10 + 1 else p1pos
    val nextp2pos = if !p1 then (p2pos + roll - 1) % 10 + 1 else p2pos
    State(
      nextp1pos,
      nextp2pos,
      if p1 then p1Score + nextp1pos else p1Score,
      if !p1 then p2Score + nextp2pos else p2Score,
      !p1
    )
  }
}

object Day21 {
  import scala.collection.mutable

  val cache = mutable.Map.empty[State, (Long, Long)]

  def main(args: Array[String]): Unit = {
    val g = new DeterministicGame(7, 5)

    var r = g.run()
    while (r._1 < 1000 && r._2 < 1000) {
      r = g.run()
    }

    val (p1, p2, rolls) = r

    val part1 = Math.min(p1, p2) * rolls

    val part2 = quantum(State(7, 5, 0, 0, true))

    println(Math.max(part2._1, part2._2))
  }

  val possibleRolls = Seq(
    3 -> 1,
    4 -> 3,
    5 -> 6,
    6 -> 7,
    7 -> 6,
    8 -> 3,
    9 -> 1
  )

  import cats.implicits.*

  def quantum(state: State): (Long, Long) = {
    state match {
      case State(_, _, p1s, p2s, _) if p1s >= 21 || p2s >= 21 =>
        if p1s > p2s then (1, 0) else (0, 1)
      case s if cache.contains(s) => cache(s)
      case s @ State(p1pos, p2pos, p1Score, p2Score, p1) =>
        val res =
          possibleRolls
            .map { case (roll, freq) =>
              quantum(state.step(roll)).bimap(_ * freq, _ * freq)
            }
            .reduce(_ |+| _)
        cache.put(s, res)
        res
    }
  }

}
