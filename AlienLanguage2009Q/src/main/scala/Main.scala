import java.io.PrintStream

import scala.io.Source

object Main {
  def solve(set: Int, l: Int, words: Array[String], itr: Iterator[String]) = {
    var str = itr.next()
    val candiSet:Array[Set[Char]] = Array.fill(l)(Set())
    (0 until l).foreach(i => {
      if (str.head == '(') {
        val arr = str.tail.takeWhile(_ != ')')
        candiSet(i) = arr.toSet
        str = str.dropWhile(_ != ')').drop(1)
      } else {
        candiSet(i) = Set(str.head)
        str = str.tail
      }
    })

    def isCompatible(word:String):Boolean = (0 until l).forall(i => candiSet(i).contains(word(i)))

    words.foldLeft(0)((sum, word) => if(isCompatible(word)) sum + 1 else sum)
  }

  def main(args: Array[String]): Unit = {
    val INPUT = "A-large-practice.in"
    val OUTPUT = INPUT.takeWhile(_ != '.') + ".out"
    val isConsole = false

    val itr = Source.fromFile(INPUT).getLines()
    val stream = if (isConsole) Console.out else new PrintStream(OUTPUT)
    try {
      val Array(l, d, n) = itr.next().split(' ').map(_.toInt)
      val words = Array.tabulate(d)(_ => itr.next())
      (1 to n).foreach { set =>
        stream.println(f"Case #$set: ${solve(set, l, words, itr)}")
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}