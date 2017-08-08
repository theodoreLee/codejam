import java.io.PrintStream
import scala.io.Source

object GWheel {
  def solve(itr: Iterator[String]) = {
    itr.next()
    itr.next()
    val p = itr.next().split(' ').map(_.toLong)
    val e = itr.next().split(' ').map(_.toLong)
    val t = itr.next().split(' ').map(_.toLong)

    def toSet(arr1:Array[Long], arr2:Array[Long]) = (for {
      a1 <- arr1
      a2 <- arr2
    } yield {
      fraction(a1,a2)
    }).toSet

    println("="*50)
    println("eSet ---- ")
    val eSet = toSet(e,e) - ((1,1))

    println("pSet----")
    val ptSet = toSet(p,t)

    println("calc---")

    val testcase = itr.next().toInt
    (0 until testcase).map { _ =>
      val Array(nom, denom) = itr.next().split(' ').map(_.toLong)
      val (gn,gd) = fraction(nom,denom)

      val e = eSet.map(x => fraction(x._1 * gn, x._2 * gd))
      ptSet.intersect(e).nonEmpty
    }.map {
      case true => "Yes"
      case false => "No"
    }.mkString("\n","\n","")

  }

  def fraction(m:Long, n:Long):(Long, Long) = {
    val g = gcd(m, n)
    (m/g, n/g)
  }

  def gcd(m: Long, n: Long): Long = n match {
      case _ if m < n => gcd(n, m)
      case _ if n == 0 => m
      case _ => gcd(n, m % n)
    }

  def main(args: Array[String]): Unit = {
    val INPUT = "B-small-practice.in"
    val OUTPUT = INPUT.takeWhile(_ != '.') + ".out2"
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