import java.io.PrintStream

import scala.io.Source

object Main {
  def gcd(m: BigInt, n: BigInt): BigInt = n match {
    case _ if m < n => gcd(n, m)
    case _ if n == 0 => m
    case _ => gcd(n, m % n)
  }

  def abs(a: BigInt) = if (a < 0) - a else a

  def solve(set: Int, itr: Iterator[String]) = {
    println(set + "=" * 50)
    val arr = itr.next().split(' ').map(BigInt(_)).tail
    val l0 = arr.head
    val list = for {
      i <- 1 until arr.length
    } yield {
      abs(l0 - arr(i))
    }
    val g = list.reduce(gcd)
    if( l0 % g == 0) 0 else g - (l0 % g)
  }

  def main(args: Array[String]): Unit = {
    val INPUT = "B-large-practice.in"
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