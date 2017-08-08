import java.io.PrintStream

import scala.io.Source

object StableNeighbors {
  val manes = Array('R', 'O', 'Y', 'G', 'B', 'V')
  // r o y g b v
  // 0 1 2 3 4 5
  val matches = Array(
    Array(3, 2, 4), //r 0
    Array(4), //o 1
    Array(5, 0, 4), //y 2
    Array(0), //g
    Array(1, 0, 2), //b
    Array(2) //v
  )

  def solve(set: Int, n: Int, unicons: Array[Int]): String = {
    var _n = n
    //first, handling to special cases, o, g and v,
    val specialCases: Array[String] = Array.fill(3)("")
    for (i <- 1 to 6 by 2) {
      if (unicons(i) > 0) {
        if (unicons(i) > unicons(matches(i).head)) {
          return "IMPOSSIBLE"
        } else if (unicons(i) == unicons(matches(i).head)) {
          if ((0 until 6).exists(j =>
            i != j && matches(i).head != j && unicons(j) > 0
          )) {
            return "IMPOSSIBLE"
          }
          return s"${manes(i)}${manes(matches(i).head)}" * unicons(i)
        } else {
          specialCases(i / 2) = s"${manes(matches(i).head)}${manes(i)}" * unicons(i) + s"${manes(matches(i).head)}"
          unicons(matches(i).head) -= unicons(i)
          _n -= unicons(i) * 2
          unicons(i) = 0
        }
      }
    }


    //second, _solve the problem before we creaeted,
    //third, replace (the first 'r', 'y', or 'b') to the special case, we created.

    def _solve(accr: List[Int], head: Int, c: Int, last: Int): List[Int] = {
      def update(i: Int) = {
        if (unicons(i) == 0) {
          Nil
        }
        else {
          unicons(i) -= 1
          _solve(i :: accr, i, c + 1, last)
        }
      }

      if (c == _n) accr
      else {
        matches(head) match {
          case m if m.length == 1 => update(m.head)
          case m if unicons(m.head) > 0 => update(m.head)
          case m =>
            if (m(1) == last && unicons(m(1)) > 0) {
              update(m(1))
            } else {
              val i = if (unicons(m(1)) > unicons(m(2))) m(1) else m(2)
              update(i)
            }
        }
      }
    }

    for {
      i <- 0 until n by 2
    } {
      if (unicons(i) > 0) {
        unicons(i) -= 1
        val ret = _solve(i :: Nil, i, 1, i)
        if (ret == Nil) {
          return "IMPOSSIBLE"
        }
        else if (!matches(ret.last).contains(ret.head)) {
          return "IMPOSSIBLE"
        } else {
          var str = ret.map(manes(_)).mkString("")
          specialCases.filter(_.nonEmpty).foreach(i => {
            str = str.replaceFirst(i.charAt(0).toString, i)
          })
          return str
        }
      }
    }
    ""
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
        val arr = itr.next().split(' ').map(_.toInt)
        stream.println(f"Case #$set: ${solve(set, arr.head, arr.tail)}")
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}