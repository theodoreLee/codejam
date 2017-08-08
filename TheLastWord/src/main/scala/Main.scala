import java.io.PrintStream
import scala.io.Source

object Main {
  def solve(in:String) = {
    in.tail.foldLeft(in.head.toString) {
      (str, c) => if(str.head <= c) c + str else str + c
    }
  }

  def main(args: Array[String]): Unit = {
    val INPUT = "A-large-practice" + ".in"
    val OUTPUT = INPUT + ".out"
    val isConsole = false

    val itr = Source.fromFile(INPUT).getLines()
    val stream = if (isConsole) Console.out else new PrintStream(OUTPUT)
    try {
      val sets = itr.next().toInt
      (1 to sets).foreach { set =>
        stream.println(f"Case #$set: ${solve(itr.next())}")
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}