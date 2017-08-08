import java.io.PrintStream

import scala.io.Source

object XXX {

  def solve(set: Int, input: String) = {
    println(set)
    var Array(hd, ad, hk, ak, b, d) = input.split(' ').map(_.toInt)
    val max = 987654321

    //    //완전 탐색
    //    //디버프를 0 ~ n 번 걸었을 때. 결과
    //    //버프를 0~ n  번 걸었을 때 결과
    def bruteForce(c: Int, chd: Int): Int = {
      if (hk <= 0) c
      else if (chd <= 0) max //기저 체력 소진 max
      else {
        var min = max
        if (ak > 0 && d > 0) {
          ak -= d
          min = Math.min(bruteForce(c + 1, chd - ak), min)
          ak += d
        }
        if (chd - ak <= 0 && chd != hd - ak) {
          min = Math.min(bruteForce(c + 1, hd - ak), min)
        }
        if (ad < hk && b > 0) {
          ad += b
          min = Math.min(bruteForce(c + 1, chd - ak), min)
          ad -= b
        }
        hk -= ad
        min = Math.min(bruteForce(c + 1, chd - ak), min)
        hk += ad
        min
      }
    }


    //Impossible Cases
    //    if (curePerGame(hd, ak - d) < 1 && (killTurnsPerGame(hk, ad) > 1)) "IMPOSSIBLE"
    //    else if (curePerGame(hd, ak - d) == 1 && killTurnsPerGame(hk, ad + b) > 1) "IMPOSSIBLE"
    //    else curePerGame(hd, ak - d)
    val ret = bruteForce(0, hd)
    if (ret >= max) "IMPOSSIBLE" else ret
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