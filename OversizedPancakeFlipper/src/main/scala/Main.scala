import java.io.PrintStream
import scala.io.Source

object Main {
  def solve(list:List[Boolean], size:Int) = {

    def _solve(list:List[Boolean], accr:Int): String =  {
      val newList = list.dropWhile(i => i)
      if (newList.isEmpty) accr.toString
      else if (newList.length < size) "IMPOSSIBLE"
      else {
        _solve(newList.take(size).map(_ == false) ++: newList.drop(size), accr + 1)
      }
    }
    _solve(list, 0)
  }

  def main(args: Array[String]): Unit = {
    val INPUT = "A-large.in"
    val OUTPUT = INPUT.takeWhile(_ != '.') + ".out"
    val isConsole = false

    val itr = Source.fromFile(INPUT).getLines()
    val stream = if (isConsole) Console.out else new PrintStream(OUTPUT)
    try {
      val sets = itr.next().toInt
      (1 to sets).foreach { set =>
        val Array(listStr, sizeStr) = itr.next().split(' ')
        val list = listStr.map(_ == '+').toList
        val size = sizeStr.toInt
        stream.println(f"Case #$set: ${solve(list, size)}")
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}