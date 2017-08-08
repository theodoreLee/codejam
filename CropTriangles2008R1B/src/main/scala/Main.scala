import java.io.PrintStream

import scala.io.Source

object Main {

  def solve(set: Int, itr: Iterator[String]) = {
    println(set, "=" * 50)
    val Array(n, a, b, c, d, x0, y0, m) = itr.next().split(' ').map(_.toInt)
    val arr = (1 until n).foldLeft(List((x0.toLong, y0.toLong)))((list, _) => {
      val (x, y) = list.head
      ((a * x + b) % m, (c * y + d) % m) :: list
    }).toArray
    var ret = 0L
    for {
      i <- 0 until n
      j <- i + 1 until n
      k <- j + 1 until n
      if (arr(i)._1 + arr(j)._1 + arr(k)._1) % 3 == 0
      if (arr(i)._2 + arr(j)._2 + arr(k)._2) % 3 == 0
    } {
       ret += 1
    }
    assert(ret > 0)
    ret
  }
  def solve2(set:Int, itr:Iterator[String]) = {
    println(set, "=" * 50)
    val Array(n, a, b, c, d, x0, y0, m) = itr.next().split(' ').map(_.toInt)

    var edgeTypes = Array.fill(9)(0L)
    edgeTypes((x0 % 3) * 3 + y0 % 3) = 1
    var X = x0.toLong
    var Y = y0.toLong
    (1 until n).foreach(_ => {
      X = (a * X + b) % m
      Y = (c * Y + d) % m
      edgeTypes(((X % 3) * 3 + Y % 3).toInt) += 1
    })
    var sum = 0L
    sum += edgeTypes.map(i => {
      if (i > 2) {(i * (i - 1) * (i - 2)) / 6} else 0
    }).sum

    for {
      i <- 0 until 9
      j <- (i + 1) until 9
      k <- (j + 1) until 9
      if (i / 3 + j / 3 + k / 3) % 3 == 0
      if (i % 3 + j % 3 + k % 3) % 3 == 0
    } {
      sum += edgeTypes(i) * edgeTypes(j) * edgeTypes(k)
    }
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
        stream.println(f"Case #$set: ${solve2(set, itr)}")
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}