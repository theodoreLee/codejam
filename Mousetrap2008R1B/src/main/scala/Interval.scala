import java.io.PrintStream
import scala.io.Source
case class Node(interval:Range) {
  var count = interval.length
  var left:Node = null
  var right:Node = null

  private var number:Int = -1

  if (this.count > 1) {
    val head = interval.head
    val tail = interval.last
    val mid = (head + tail) / 2
    left = Node(head to mid)
    right = Node(mid + 1 to tail)
  } else {
    assert(this.count == 1)
  }

  def add(count: Int, number: Int, last:Int):Int = {
    if (interval.length == 1) {
      assert(this.count == 1, s"${this.count} ${this.number} $count $number")
      this.count = 0
      this.number = number
      0
    } else {
      var _count = count + last
      while(this.count < _count) {
        _count -= this.count
      }
      this.count -= 1
      if (this.left.count >= _count && this.left.count != 0 ) {
        this.left.add(_count, number, 0)
      } else {
        this.left.count + this.right.add(_count - this.left.count, number, 0)
      }
    }
  }

  def getList:List[Int] = if(interval.length == 1) {
    this.number :: Nil
  } else {
    this.left.getList ::: this.right.getList
  }

  def value(idx:Int):Int =
    if(interval.length == 1) this.number
    else if(this.left.interval.contains(idx)) this.left.value(idx)
    else this.right.value(idx)
}

object Interval {
  def perfect(k:Int) = {
    val head = Node(1 to k)
    (1 to k).foldLeft(0)((ptr, i) => head.add(i, i, ptr))
    head
  }

  def solve(set: Int, itr: Iterator[String]) = {
    println(set + "=" * 50)
    val k = itr.next().toInt
    val arr = itr.next().split(' ').tail.map(_.toInt)
    val perf = perfect(k)
    arr.map(perf.value).mkString(" ")
  }

  def main(args: Array[String]): Unit = {
    val INPUT = "C-large-practice.in"
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