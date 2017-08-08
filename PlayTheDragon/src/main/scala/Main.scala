import java.io.PrintStream

import scala.io.Source


object Main {
  def rt(a: Int, b: Int, c: Int) = {
    val con = Math.sqrt(Math.abs(b * b - 4 * a * c))
    val r1 = (-b + con) / (2 * a)
    val r2 = (-b - con) / (2 * a)
    if (r2 > 0) Math.ceil(r2).toInt else if (r1 > 0) Math.ceil(r1).toInt else 0
  }

  def f(n: Int, k: Int) = {
    if (n < 3 || k < 2) n
    else n + (n - 3) / (k - 1)
  }

  def cureTurn(ohd: Int, ak: Int) = if (ak <= 0) 987654321 else (ohd - 1) / ak

  def dTurns(d: Int, dh: Int, ka: Int, i: Int) = {
    if (d == 0) 0
    else {
      if (i == 0) {
        Math.ceil(ka.toDouble / d).toInt
      } else {
        (ka - (dh - 1) / i) / d // (dh - 1) /(ka - d * d') = i
      }
    }
  }

  def solve(set: Int, input: String) = {
    var Array(ohd, ad, ohk, ak, b, d) = input.split(' ').map(_.toInt)
    val buffTurns = rt(b, ad, ohk)
    val newAd = ad + b * buffTurns
    val killTurns = Math.ceil(ohk.toDouble / newAd).toInt + buffTurns
    val oPerTurn = cureTurn(ohd, ak)
    val oMaxTurn = f(killTurns, oPerTurn)

    if (killTurns == 1) 1
    //how many times do we debuff and cure?
    else if (cureTurn(ohd, ak - d) == 0) "IMPOSSIBLE"
    // d == 0 이면이 아니라 cureTurn으로 계산해야 할듯 한데 이상함.
    else if (oPerTurn == 1 && killTurns > 2 && d == 0) "IMPOSSIBLE"
    else if (d == 0) oMaxTurn
    else {
      var _ak = ak
      var min = oMaxTurn
      var k = killTurns
      if (oPerTurn == 0) {
        min = f(k + 1, cureTurn(ohd, ak - d))
      }
      var last = min
      var i = 0
      var hd = ohd
      while (last <= min) {
        i += 1
        _ak -= d
        if (hd - _ak <= 0) {
          i += 1
          hd = ohd - _ak
          last = f(k + 1, cureTurn(ohd, _ak)) + i - 1
        } else {
          //여기서 틀릴듯 한데
          last = f(k, cureTurn(ohd, _ak)) + i - 1
        }
        if (last < min) min = last
      }
      min
    }
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