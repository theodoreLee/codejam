import java.io.PrintStream

import scala.collection.mutable
import scala.io.Source

object Main {
  case class Circle(set:Set[Int], cricle:List[Int])
  def solve(itr: Iterator[String]) = {
    val n = itr.next().map(_.toInt)
    val children = itr.next().split(' ').map(i => (i.toInt, false))
    var map = mutable.Map[Int,List[Int]]().withDefaultValue(Nil)
    for (
      i <- 0 until n
    ) {
      val (bff, traversed) = children(i)
      map(bff) = i :: map(bff)
      if (!traversed) {
        var next = i
        val set = mutable.Set[Int]()
        var list:List[Int] = Nil
        while(!set.contains(next) && !children(next)._2) {
          set += i
          list = i :: list

          next = children(i)._1

        }
      }

    }
  }

  def main(args: Array[String]): Unit = {
    val INPUT = "test.in"
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