import java.io.PrintStream
import scala.io.Source

object Main {
  def solve(set: Int, itr: Iterator[String]) = {
    val Array(p, k, l) = itr.next().split(' ').map(_.toInt)
    //p => the maximum number of letters to place on a Key
    //k => the number of keys available
    //l => the number of letters in our alphabet
    println(set +"="*50)
    val ls = itr.next().split(' ').map(_.toInt).sorted(Ordering[Int].reverse)
    var sum = 0L
    var i = 1
    var j = 0
    ls.foreach(frequency => {
      assert(frequency > 0)
      if(j == k) {
        i += 1
        j = 0
      }
      sum += i * frequency
      j += 1
    })
    assert(sum > 0)
    sum
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
