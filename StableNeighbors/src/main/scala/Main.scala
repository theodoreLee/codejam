import java.io.PrintStream

import scala.io.Source

object Main {
  val maneText = Array("R", "O", "Y", "G", "B", "V")
  // r o y g b v
  // 0 1 2 3 4 5
  val matches = Array(
    4, //o 1
    0, //g
    2 //v
  )

  def solve(set: Int, input: String): String = {
    val builder = StringBuilder.newBuilder
    val arr = input.split(" ").map(_.toInt)
    val n = arr.head
    val manes = arr.tail

    var last = "X"
    var first = "X"
    var colors = Map("R" -> manes(0), "Y" -> manes(2), "B" ->  manes(4))
    val rep = Array.fill(3)("")
    for {
      mixed <- 1 until 6 by 2
    } {
      if (manes(mixed) > 0) {
        val op = matches(mixed / 2)
        if(manes(mixed) == manes(op)) {
          if( (0 until 6).exists(j => j != mixed && j != op && manes(j) != 0)) return "IMPOSSIBLE"
          else return (maneText(op) + maneText(mixed)) * manes(mixed)
        } else if (manes(mixed) < manes(op)) {
          rep(mixed/2) = (maneText(op) + maneText(mixed)) * manes(mixed) + maneText(op)
          colors += (maneText(op) -> (colors(maneText(op)) - manes(mixed)))
        } else return "IMPOSSIBLE"
      }
    }
    
    while (colors.values.max > 0) {
      if (set == 3) println(colors)
      last = {
        val (key, qty) = (colors - last).maxBy(_._2)
        if (last != first && colors(first) > 0) first
        else key
      }
      if (colors(last) == 0) return "IMPOSSIBLE"

      if (first == "X") first = last
      builder.append(last)
      colors += (last -> (colors(last) - 1))
    }
    var ret = builder.toString
    if (ret.head == ret.last) "IMPOSSIBLE" else {
      rep.filter(_.nonEmpty).foreach(i => ret = ret.replaceFirst(i.charAt(0).toString, i))
      ret
    }
  }

  def main(args: Array[String]): Unit = {
    val INPUT = "B-large-practice.in"
    val OUTPUT = INPUT.takeWhile(_ != ".") + ".out"
    val isConsole = false

    val itr = Source.fromFile(INPUT).getLines()
    val stream = if (isConsole) Console.out else new PrintStream(OUTPUT)
    try {
      val sets = itr.next().toInt
      (1 to sets).foreach { set =>
        stream.println(f"Case #$set: ${solve(set, itr.next())}")
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}