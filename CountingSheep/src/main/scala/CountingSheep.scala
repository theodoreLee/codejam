import java.io.PrintStream
import scala.io.Source

object CountingSheep {
  def multiple(number:String, set:Set[Char], accr:Int, ret:Long):Long = set.size match {
    case 10 => ret
    case _ =>
      val ret = number.toLong * accr
      multiple(number, set ++ ret.toString.toSet, accr + 1, ret)
  }
  def solve(number: String) = {
    println("-" * 50)
    val initialSet = number.toSet
    if(initialSet.size == 1 && initialSet.head == '0') "INSOMNIA"
    else multiple(number, initialSet, 2, number.toLong)
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
        stream.println(f"Case #$set: ${solve(itr.next())}")
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}