import java.io.PrintStream

import scala.collection.mutable
import scala.io.Source

object Small {
  def solve(set: Int, itr: Iterator[String]) = {
    println(set, "="*50)
    val Array(r, k, n) = itr.next().split(' ').map(_.toInt)
    val groups = itr.next().split(' ').map(_.toInt)
    val queue = collection.mutable.Queue(groups : _ *)
    val restQ:mutable.Queue[Int] = collection.mutable.Queue()
    val memo = mutable.Map[List[Int], (Int,Int)]()

    def _solve(i:Int, sum:Int) :Int = {
      var seats = 0
      val list = queue.toList
      if(i == 0) {
        sum
      } else if(memo.get(list).nonEmpty && memo(list)._1 - i == 1) {
        val (_r, _sum) = memo(list)
        val delta =  _r - i
        println(_r, sum, i, sum)
        memo.clear()
       _solve(r % delta, sum + (i / delta) * (sum - _sum))
      } else {
        memo += (list -> (i,sum))
        while (queue.nonEmpty && seats + queue.head <= k) {
          seats += queue.head
          restQ.enqueue(queue.dequeue())
        }
        queue.enqueue(restQ : _ *)
        restQ.clear()
        _solve(i - 1, sum + seats)
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