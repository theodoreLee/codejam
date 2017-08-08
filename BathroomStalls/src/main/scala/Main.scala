import java.io.PrintStream

import scala.io.Source

object Main {
  def solve(stalls: Long, people: Long): (Long, Long) = {
    def _solve(n: Long, k: Long): (Long, Long) = {
      if (n == k) (0L, 0L)
      else if (k == 1) {
        (n / 2, (n - 1) / 2)
      }
      else {
        val mod = k % 2
        if ((n - 1) % 2 == 0) _solve((n - 1) / 2, k / 2)
        else {
          val newN = n / 2 - mod
          val newK = k / 2
          _solve(newN, newK)
        }
      }
    }

    _solve(stalls, people)
  }

  def main(args: Array[String]): Unit = {
    val INPUT = "C-large.in"
    val OUTPUT = INPUT.takeWhile(_ != '.') + ".out"
    val isConsole = false

    val itr = Source.fromFile(INPUT).getLines()
    val stream = if (isConsole) Console.out else new PrintStream(OUTPUT)
    try {
      val sets = itr.next().toInt
      (1 to sets).foreach { set =>
        val Array(n, k) = itr.next().split(' ').map(_.toLong)
        val (max, min) = solve(n, k)
        stream.println(f"Case #$set: $max $min")
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}