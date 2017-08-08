import java.io.PrintStream
import scala.io.Source

object Main {
  def solve(set: Int, itr: Iterator[String]) = {
    val n= itr.next().toInt
    val x = itr.next().split(' ').map(_.toLong).sorted
    val y = itr.next().split(' ').map(_.toLong).sorted(Ordering[Long].reverse)
    var ret = BigInt(0)
    (0 until n).foreach(i => ret += x(i) * y(i))
    ret
  }

  def main(args: Array[String]): Unit = {
    val INPUT = "A-large-practice.in"
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