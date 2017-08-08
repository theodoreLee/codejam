import java.io.PrintStream
import scala.io.Source

object Main {
  def solve(set: Int, itr: Iterator[String]) = {
    val Array(n, k) = itr.next().split(' ').map(_.toInt)

    val pow = Math.pow(2, n).toInt
    assert(pow > 0)
    if((k + 1) % pow == 0) "ON" else "OFF"
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