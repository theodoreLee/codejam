import java.io.PrintStream

import scala.collection.mutable
import scala.io.Source

case class Pos(cd: Int, ad: Int, ck: Int, ak: Int)

object Small {
  def solve(set: Int, input: String) = {
    var Array(ohd, ad, ohk, ak, b, d) = input.split(' ').map(_.toInt)
    println(set)

    case class Stage(hd: Int, ad: Int, hk: Int, ak: Int, n: Int) extends Ordered[Stage] {
      override def compare(that: Stage): Int = that.n.compare(n)

      def pos = Pos(hd, ad, hk, ak)

      def attack = Stage(if (hk - ad > 0) hd - ak else ohd, ad, hk - ad, ak, n + 1)

      def buff = {
        val nd = Math.min(ad + b, ohk)
        Stage(hd - ak, nd, hk, ak, n + 1)
      }

      def cure = Stage(ohd - ak, ad, hk, ak, n + 1)

      def debuff = {
        val nd = Math.max(ak - d, 0)
        Stage(hd - nd, ad, hk, nd, n + 1)
      }
    }

    val first = Stage(ohd, ad, ohk, ak, 0)
    val seen = mutable.Set(first.pos)
    val q = mutable.PriorityQueue(first)

    def doTry(stage: Stage) = {
      if (stage.hd > 0 && seen.add(stage.pos)) q.enqueue(stage)
    }

    var ret = -1
    while (q.nonEmpty) {
      val head = q.dequeue()
      if (head.hk <= 0) {
        ret = head.n
        q.clear()
      } else {
        doTry(head.attack)
        doTry(head.cure)
        doTry(head.buff)
        doTry(head.debuff)
      }
    }

    if (ret == -1) "IMPOSSIBLE" else ret
  }

  def main(args: Array[String]): Unit = {
    val INPUT = "C-small-practice.in"
    val OUTPUT = INPUT.takeWhile(_ != '.') + ".out"
    val isConsole = false

    val itr = Source.fromFile(INPUT).getLines()
    val stream = if (isConsole) Console.out else new PrintStream(OUTPUT)
    try {
      val sets = itr.next().toInt
      (1 to sets).foreach { set =>
        stream.println(f"Case #$set: ${solve(set: Int, itr.next())}")
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}