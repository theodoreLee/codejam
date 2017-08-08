import java.io.PrintStream

import scala.collection.mutable
import scala.io.Source

object Main {
  def solve(set: Int, itr: Iterator[String]) = {
    println(set, "=" * 50)
    val Array(r, k, n) = itr.next().split(' ').map(_.toInt)
    val groups = itr.next().split(' ').map(_.toInt)
    val len = groups.length
    var ptr = 0
    var paths:mutable.Queue[(Int,Int)] = mutable.Queue() //ptr , riders.

    def _solve(i: Int, sum: Long): Long = {
      if (i == 0) sum
      else {
        if (i != r && paths.exists(_._1 == ptr)) {
          paths = paths.dropWhile(_._1 != ptr)
          val _sum = paths.map(_._2.toLong).sum
          var _i = i
          var ret:Long = sum
          ret += _sum * (_i / paths.length)
          _i = _i % paths.length
          while(_i > 0) {
            _i -= 1
            ret += paths.dequeue()._2
          }
          ret
        } else {
          var riders = 0
          var j = 0
          var p = ptr
          while (j < len && riders + groups(p) <= k) {
            riders += groups(p)
            p += 1
            j += 1
            if (p == len) p = 0
          }
          paths.enqueue((ptr, riders))
          ptr = p
          _solve(i - 1, sum + riders)
        }
      }
    }

    _solve(r, 0)
  }

  def main(args: Array[String]): Unit = {
    val INPUT = "C-large-practice.in"
    val OUTPUT = INPUT.takeWhile(_ != '.') + ".out"
    val isConsole = false

    val itr = Source.fromFile(INPUT).getLines()
    val stream = if (isConsole) Console.out else new PrintStream(OUTPUT)
    try {
      val sets = itr.next().toInt
      (1 to sets).foreach { set =>
        stream.println(f"Case #$set: ${solve(set, itr)}")
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}