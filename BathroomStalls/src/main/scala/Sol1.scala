import java.io.PrintStream

import scala.io.Source

object Sol1 {

  def solve(t: Int, stalls: Long, people: Long): (Long, Long) = {
    var _n = stalls
    var _k = people
    if (_n == _k) return (0L, 0L)
    val pq = collection.mutable.PriorityQueue(_n)
    var max = 0L
    var min = 0L
    while (_k > 0) {
      val i = pq.dequeue()
      max = i / 2
      min = (i / 2) - ((i + 1) % 2)
      if (max == 0) return (0L, 0L)
      pq += max
      if (min > 0) pq += min
      _k -= 1
    }
    (max, min)
  }

  def main(args: Array[String]): Unit = {
    val INPUT = "C-small-2-attempt0.in"
    val OUTPUT = INPUT.takeWhile(_ != '.') + ".out"
    val isConsole = false

    val itr = Source.fromFile(INPUT).getLines()
    val stream = if (isConsole) Console.out else new PrintStream(OUTPUT)
    try {
      val sets = itr.next().toInt
      (1 to sets).foreach { set =>
        val Array(n, k) = itr.next().split(' ').map(_.toLong)
        val (max, min) = solve(set, n, k)
        stream.println(f"Case #$set: $max $min")
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}