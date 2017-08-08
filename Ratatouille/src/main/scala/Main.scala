import java.io.PrintStream

import scala.io.Source

object Main {

  def solve(set: Int, n: Int, p: Int, pack: Array[Int], ingredient: Array[Array[Double]]) = {
    val minPack = pack.map(_ * 0.9)
    val maxPack = pack.map(_ * 1.1)
    var sum = 0
    val ranges = Vector.tabulate(n, p) {
      case (i, j) =>
        // 0.9 * r * m < q < 1.1 * r * m
        // q / r * 10 / 11 < m < q / r * 10 / 9
        // t / 11 < m < t / 9
        val t = ingredient(i)(j) / pack(i) * 10
        val min = Math.ceil(t / 11).toInt
        val max = Math.floor(t / 9).toInt
        (min to max).toSet
    }.toArray

    def findPairs(range: Set[Int], c: Int, j: Int, accr: List[(Int, Int)]): List[(Int, Int)] = {
      if (c == n) accr
      else if (j == ranges(c).length) Nil
      else {
        val newRange = range intersect ranges(c)(j)
        if (newRange.nonEmpty) {
          findPairs(newRange, c + 1, 0, (c, j) :: accr)
        } else {
          findPairs(newRange, c, j + 1, accr)
        }
      }
    }

    while (ranges(0).nonEmpty) {
      val range = ranges(0).head
      if (range.nonEmpty) {
        val ret = findPairs(range, 1, 0, (0, 0) :: Nil)
        if (ret == Nil) {
          ranges(0) = ranges(0).tail
        } else {
          sum += 1
          for (
            (i, j) <- ret
          ) {
            ranges(i) = ranges(i).slice(0, j) ++: ranges(i).slice(j + 1, ranges(i).length)
          }
        }
      } else {
        ranges(0) = ranges(0).tail
      }
    }
    sum
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
        val Array(n, p) = itr.next().split(' ').map(_.toInt)
        val pack = itr.next().split(' ').map(_.toInt)
        val ingredient = (0 until n).toArray.map(_ => itr.next().split(' ').map(_.toDouble).sorted)
        println(set + "-" * 30)
        stream.println(f"Case #$set: ${solve(set, n, p, pack, ingredient)}")
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}