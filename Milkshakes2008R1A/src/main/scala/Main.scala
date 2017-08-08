import java.io.PrintStream
import scala.io.Source

case class Customer(prefer: Array[(Int, Int)]) {
  val meltedItem: Option[Int] = prefer.find(_._2 == 1).map(_._1)
  var satisfied = false
  def isSatisfied(arr:Array[Int]) = satisfied || prefer.exists( i => arr(i._1) == i._2)
}

object Main {
  def solve(set: Int, itr: Iterator[String]) = {
    val n = itr.next().toInt
    val m = itr.next().toInt
    val customers = (0 until m).map(_ => {
      val arr = itr.next().split(' ').map(_.toInt)
      val head = arr.head
      val tail = arr.tail
      val preference = Array.tabulate(head)(i => (tail(i * 2) - 1,tail(i * 2 + 1)))
      Customer(preference)
    })
    def _solve (i: Int, arr:Array[Int]):Array[Int] = {
      if (i == m) arr
      else if (customers(i).isSatisfied(arr)) _solve(i + 1, arr)
      else if (customers(i).meltedItem.isDefined) {
        arr(customers(i).meltedItem.get) = 1
        customers(i).satisfied = true
        _solve(0, arr)
      } else {
        Array.fill(n)(-1)
      }
    }
    val ret = _solve(0, Array.fill(n)(0))
    if(ret(0) == -1) "IMPOSSIBLE"
    else ret.mkString(" ")
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