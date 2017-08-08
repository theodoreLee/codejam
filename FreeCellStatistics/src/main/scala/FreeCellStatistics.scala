import java.io.FileOutputStream
import scala.io.Source

object FreeCellStatistics {

  def solve(line:String) = {
    val Array(n, pd, pg) = line.split(' ').map(_.toLong)

    val gcdPd = gcd(pd, 100)
    val minimumTodayTotal = 100 / gcdPd

    //!(minimumTodayTotal > n || (pd > 0 && pg == 0) || (pd != 100 && pg == 100))
    val ret = minimumTodayTotal <= n && (pd <= 0 || pg != 0) && (pd == 100 || pg != 100)
    if(ret) "Possible" else "Broken"
  }

  def gcd(m: Long, n: Long): Long = n match {
    case _ if m < n => gcd(n, m)
    case _ if n == 0 => m
    case _ => gcd(n, m % n)
  }

  def main(args: Array[String]): Unit = {
    val INPUT = "A-large-practice.in"
    val OUTPUT = INPUT.takeWhile(_ != '.') + ".out"
    val isConsole = false

    val itr = Source.fromFile(INPUT).getLines()
    val stream = if (isConsole) Console.out else new FileOutputStream(OUTPUT)
    try {
      Console.withOut(stream) {
        val sets = itr.next().toInt
        (1 to sets).foreach { set =>
          println(f"Case #$set: ${solve(itr.next())}")
        }
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}