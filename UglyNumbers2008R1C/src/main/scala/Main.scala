import java.io.PrintStream

import scala.io.Source

object Main {
  def solve(set: Int, itr: Iterator[String]) = {
    println(set + "=" * 50)
    val str = itr.next()
    val des = Array.fill(210)(0)
    val len = str.length - 1

    def add(a: BigInt, b: BigInt, plus: Boolean) = {
      val _b = if (plus) b else -b
      val ret = if (a + _b > 0) a + _b else -(a + _b)
      ret
    }

    def abs(a: BigInt) = if (a < 0) -a else a

    def ugly(i: Int, before: BigInt, res: BigInt, plus: Boolean): Unit = i match {
      case _ if i == str.length =>
        val ret = add(res, before, plus)
        val r = Math.abs((ret % 210).toInt)
        des(r) = des(r) + 1
      case _ =>
        val c = str(i) - '0'
        ugly(i + 1, before * 10 + c, res, plus)
        ugly(i + 1, c, add(res, before, plus), true)
        ugly(i + 1, c, add(res, before, plus), false)
    }

    if (str.length == 1) {
      val c = str.toInt
      if (c % 2 == 0 || c % 3 == 0 || c % 5 == 0 || c % 7 == 0) 1 else 0
    } else {
      ugly(1, str(0) - '0', 0, true)
      var sum = 0L
      for (
        i <- des.indices
        if des(i) > 0
        if i % 2 == 0 || i % 3 == 0 || i % 5 == 0 || i % 7 == 0
      ) {
        sum += des(i)
      }
      sum
    }
  }

  def isUgly(i:Int) = i % 2 == 0 || i % 3 == 0 || i % 5 == 0 || i % 7 == 0

  def dp(set: Int, itr: Iterator[String]) = {
    println(set + "=" * 50)
    val MAX_LEN = 13
    val str = itr.next()
    val des = Array.fill(MAX_LEN, 210)(0)
    if(str.length == 1) {
      isUgly(str.toInt)
    } else {
      for {
        i <- 0 until str.length
      } {

      }
    }
  }

  def main(args: Array[String]): Unit = {
    val INPUT = "B-small-practice.in"
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