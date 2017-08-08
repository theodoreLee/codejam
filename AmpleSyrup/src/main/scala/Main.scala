import java.io.PrintStream

import scala.io.Source

case class Pancake(r: Int, h: Int) {
  val hr = 2 * Math.PI * r * h
  val maxSur = Math.PI * r * r + hr
}

object Main {
  def calc(pancakes: List[Pancake]) = {
    val sorted = pancakes.sortBy(_.r)
    sorted.head.maxSur + sorted.drop(1).map(_.hr).sum
  }

  def solve(itr: Iterator[String]) = {
    val Array(n, k) = itr.next().split(' ').map(_.toInt)
    val pancakes = (0 until n).map(_ => {
      val Array(r, h) = itr.next().split(' ').map(_.toInt)
      Pancake(r, h)
    }).sortBy(_.r).reverse

    //요거 아닐듯
    //    val takes = pancakes.take(k).sortBy(_.maxSur).reverse
    //    takes.head.maxSur + takes.slice(1, k - 1 + 1).map(_.hr).sum
    def bruteForce(i: Int, take: List[Pancake], k: Int): Double = {
      if (k == 0) calc(take)
      else {
        var max: Double = -1
        for {
          p <- i until pancakes.length
        } {
          max = Math.max(bruteForce(i + 1, pancakes(i) :: take, k - 1), max)
          max = Math.max(bruteForce(i + 1, take, k), max)
        }
        max
      }
    }

    bruteForce(0, Nil, k)
  }

  def main(args: Array[String]): Unit = {
    val INPUT = "A-small-attempt0.in"
    val OUTPUT = INPUT.takeWhile(_ != '.') + ".out"
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