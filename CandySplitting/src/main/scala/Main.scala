import java.io.PrintStream
import scala.io.Source

object Main {
  def xor(x:Int, y:Int):Int = x ^ y

  def solve(set: Int, itr: Iterator[String]) = {
    val arr = itr.next().split(' ').map(_.toInt)
    if(arr.foldLeft(0)(xor) != 0) "NO"
    else {
      arr.sum - arr.min
    }
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
        itr.next()
        stream.println(f"Case #$set: ${solve(set, itr)}")
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}
