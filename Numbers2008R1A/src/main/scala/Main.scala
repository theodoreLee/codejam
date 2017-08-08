import java.io.PrintStream
import scala.io.Source

object Main {
  /** Newton-Raphson method
    * before 근사값 root2 => 2 root => 3.
    * n 유효자릿수.
    * x 는 구하려는 값
   */
  def sqrt(before:BigDecimal, n:Int, x:Int):BigDecimal = {
    if(n == 0) {
      before
    } else {
      sqrt((before + x / before)/2, n - 1, x)
    }
  }

  val sqrt5 = sqrt(3, 90, 5)
  var cur = BigDecimal(1)
  val x = 3 + sqrt5
  val arr  = Array.tabulate(30)(_ => {
    cur = cur * x
    (cur % 1000).toInt.toString
  })
  def solve(set: Int, itr: Iterator[String]) = {
    val n = itr.next().toInt
    println(sqrt5)
    "0" * (3 - arr(n - 1).length) + arr(n-1)
  }


  def main(args: Array[String]): Unit = {
    val INPUT = "C-small-practice.in"
    val OUTPUT = INPUT.takeWhile(_ != '.') + ".out3"
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