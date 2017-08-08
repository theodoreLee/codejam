import java.io.PrintStream
import scala.io.Source

object RevengeOfThePancakes {
  def solve(str:String) = {
    val list = str.toVector.map(_ == '+')
    println("-"* 50)
    def _solve(list:Vector[Boolean], accr:Int):Int = list.lastIndexOf(false) match {
      case -1 => accr
      case x if !list.head => _solve(list.take(x + 1).reverse.map(_ == false), accr + 1)
      case x =>
        val change = list.takeWhile(_ == true)
        _solve(change.map(_ => false) ++ list.drop(change.length), accr + 1)
    }
    _solve(list, 0)
  }

  def main(args: Array[String]): Unit = {
    val INPUT = "B-large.in"
    val OUTPUT = INPUT.takeWhile(_ != '.') + ".out"
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