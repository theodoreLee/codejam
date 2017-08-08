import java.io.PrintStream
import scala.io.Source

object Main {
  def solve(itr: Iterator[String]) = {
    val n = itr.next().toInt
    val arr = Array.fill(2501)(0)
    (0 until 2 * n - 1).foreach(_ => itr.next().split(' ').foreach(i => arr(i.toInt) += 1))
    (1 until 2501).filter(i => arr(i) % 2 == 1).mkString(" ")
  }

  def main(args: Array[String]): Unit = {
    val INPUT = "B-large-practice" + ".in"
    val OUTPUT = INPUT.takeWhile(_ != '.') + ".out"
    val isConsole = false

    val itr = Source.fromFile(INPUT).getLines()
    val stream = if (isConsole) Console.out else new PrintStream(OUTPUT)
    try {
      val sets = itr.next().toInt
      (1 to sets).foreach { set =>
        stream.println(f"Case #$set: ${solve(itr)}")
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}