import java.io.PrintStream

import scala.io.Source

object Main {
  def primes(max: Int): List[Int] = {
    val arr = Array.fill(max + 1)(true)
    val root = Math.sqrt(max).toInt
    (2 to root).foreach(i => (i * 2 to max by i).foreach(arr(_) = false))
    var ret: List[Int] = Nil
    (max to 2 by -1).foreach(i => if (arr(i)) ret = i :: ret)
    ret
  }

  def solve(set: Int, itr: Iterator[String]) = {
    //    println("solve1 : " + set + "="* 50)
    val Array(a, b, p) = itr.next().split(' ').map(_.toInt)
    val prime = primes(b - a + 1).dropWhile(_ < p)
    var arraySet: List[Set[Int]] = Nil
    prime.foreach(_p => {
      val _a = if (a % _p == 0) a else a + _p - a % _p
      assert(_a % _p == 0)
      val set = (_a to b by _p).toSet
      if (set.nonEmpty) {
        val (left, right) = arraySet.partition(_.intersect(set).nonEmpty)
        arraySet = left.foldLeft(set)(_ union _) :: right
      }
    })
    b - a + 1 - arraySet.map(_.size).sum + arraySet.length
  }

  def find(arr: Array[Int], i: Int, accr: List[Int] = Nil): Int = {
    if (arr(i) == -1) i
    else {
      arr(i) = find(arr, arr(i), i :: accr)
      arr(i)
    }
  }

  //union find 쓰고 싶어도. 같은 i 번째에 동일한 prime을 가지는 합성수가 존재함..
  def solve2(set: Int, itr: Iterator[String]) = {
    println(set + "=" * 50)
    val Array(a, b, p) = itr.next().split(' ').map(_.toLong)
    val ret: Array[Int] = Array.fill((b - a).toInt + 1)(-1)
    val prime = primes((b - a).toInt + 1).dropWhile(_ < p)
    val sets = Array.fill(prime.length)(-1)
    var ptr = 0
    prime.foreach(_p => {
      val _a = if (a % _p == 0) a else a + _p - a % _p
      (_a to b by _p).foreach(i => {
        val idx = (i - a).toInt
        if (ret(idx) == -1) ret(idx) = ptr
        else {
          val root = find(sets, ret(idx))
          if(root != ptr) sets(root) = ptr
        }
      })
      ptr += 1
    })
    ret.count(_ == -1) + sets.count(_ == -1)
  }

  def isPrime(x: Long, pr: List[Int]): Boolean = pr.forall(i => x <= i || x % i != 0)

  def main(args: Array[String]): Unit = {
    val INPUT = "B-large-practice.in"
    val OUTPUT = INPUT.takeWhile(_ != '.') + ".out"
    val isConsole = false

    val itr = Source.fromFile(INPUT).getLines()
    val stream = if (isConsole) Console.out else new PrintStream(OUTPUT)
    try {
      val sets = itr.next().toInt
      //      val p = primes(1000000)
      (1 to sets).foreach { set =>
        stream.println(f"Case #$set: ${solve2(set, itr)}")
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}