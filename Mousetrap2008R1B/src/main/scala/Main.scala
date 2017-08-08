import java.io.PrintStream

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Main {
  def perfect(k:Int):Array[Int] = {
    val ret = Array.fill(k + 1)(0)
    var ptr = 0
    var lr = 0
    for (
      i <- 1 to k
    ) {
      val res = k - i + 1
      var cnt = if (res >= i) 0 else res * ((i -1) / res)
      if (cnt == 0 && res - lr < i) {
        cnt = res - lr
        lr = 0
        ptr = 1
      }

      while(cnt < i) {
        ptr += 1
        if (ptr == k + 1)  {
          lr = 0
          ptr = 1
        }
        if (ret(ptr) == 0) {
          cnt += 1
          lr += 1
        }
      }
      assert(ret(ptr)== 0)
      ret(ptr) = i
      lr -= 1
    }
    ret
  }

  def perfect2(k:Int):Array[Int] = {
    val ret = Array.fill(k + 1)(0)
    val arrayBuffer = ArrayBuffer.tabulate(k)(_ + 1)
    var ptr = 0
    for (
      i <- 1 to k
    ) {
      val length = arrayBuffer.length
      if(ptr + i - 1 < length) {
        ptr += i - 1
        ret(arrayBuffer(ptr)) = i
        arrayBuffer(ptr) = -1
        arrayBuffer.remove(ptr)
      } else {
        val delta = (ptr + i - 1 - length) % length
        ptr = delta
        ret(arrayBuffer(ptr)) = i
        arrayBuffer.remove(ptr)
      }
    }
    ret
  }

  def solve(set: Int, itr: Iterator[String]) = {
    println(set + "=" * 50)
    val k = itr.next().toInt
    val arr = perfect2(k)
    itr.next().split(' ').tail.map(i => arr(i.toInt)).mkString(" ")
  }

  def main(args: Array[String]): Unit = {
    val INPUT = "test.2.large.in"
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