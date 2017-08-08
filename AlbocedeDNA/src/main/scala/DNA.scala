import java.io.PrintStream
import scala.io.Source

object DNA {
  def solve(dna: String) = {

    val array = new Array[(Int,Int,Int,Int)](dna.length)
    def getSizeOf(idx:Int, a:Int = 0, b:Int = 0, c:Int = 0, d:Int = 0):Unit = idx match {
      case -1 => Unit
      case _ =>
        dna(idx) match {
          case 'a' =>
            array.update(idx, (a +1 , b, c, d))
            getSizeOf(idx -1, a + 1, b, c , d)
          case 'b' =>
            array.update(idx, (a, b + 1, c, d))
            getSizeOf(idx -1, a , b + 1, c , d)
          case 'c' =>
            array.update(idx, (a, b, c + 1, d))
            getSizeOf(idx -1, a, b, c + 1 , d)

          case 'd' =>
            array.update(idx, (a, b, c, d + 1))
            getSizeOf(idx -1, a, b, c, d + 1)
        }
    }
    getSizeOf(dna.length -1)

//    println(dna)
//    array.foreach((x) => println(x._1, x._2, x._3, x._4))

    case class Accr(char:Option[Char], idx:Int, a:Int =0, b:Int =0, c:Int =0, d:Int = 0) {
      def add(char:Option[Char]):(Accr,Accr) = char match {
          case Some('a') =>
            (this.copy(char, idx = this.idx + 1, a = a + 1), this.copy(idx = this.idx + 1))
          case Some('b') =>
            (this.copy(char, idx = this.idx + 1, b = b + 1), this.copy(idx = this.idx + 1))
          case Some('c') =>
            (this.copy(char, idx = this.idx + 1, c = c + 1), this.copy(idx = this.idx + 1))
          case Some('d') =>
            (this.copy(char, idx = this.idx + 1, d = d + 1), this.copy(idx = this.idx + 1))
        }
    }

    def _solve(accr:Vector[Accr], completed:Vector[Accr]):Vector[Accr] = accr match {
      case Vector() => completed
      case x +: xs if x.idx == dna.length  => _solve(xs, completed)
      case x +: xs =>
        (dna(x.idx), x.char) match {
          case ('a', None) =>
            val (newX, newX2) = x.add(Some('a'))
            _solve(xs :+ newX :+ newX2, completed)
          case ('a', Some('a')) =>
            val (newX, newX2) = x.add(Some('a'))
            _solve(xs :+ newX :+ newX2, completed)
          case ('b', Some('a')) =>
            val (newX, newX2) = x.add(Some('b'))
            _solve(xs :+ newX :+ newX2, completed)
          case ('b', Some('b')) =>
            val (newX, newX2) = x.add(Some('b'))
            _solve(xs :+ newX :+ newX2, completed)
          case ('c', Some('b')) =>
            val (newX, newX2) = x.add(Some('c'))
            _solve(xs :+ newX :+ newX2, completed)
          case ('c', Some('c')) =>
            val (newX, newX2) = x.add(Some('c'))
            _solve(xs :+ newX :+ newX2, completed)
          case ('d', Some('c')) =>
            val (newX, newX2) = x.add(Some('d'))
            _solve(xs :+ newX :+ newX2, completed :+ newX)
          case ('d', Some('d')) =>
            val (newX, newX2) = x.add(Some('d'))
            _solve(xs :+ newX :+ newX2, completed :+ newX)
          case ('a', Some('d')) if dna.drop(x.idx + 1).toSet.size < 4 => _solve(xs , completed)
          case ('a', Some('d')) =>
            val (newX, newX2) = x.add(Some('a'))
            if(x.a == x.c && x.b == x.d)
              _solve(xs :+ newX :+ newX2, completed)
            else
              _solve(xs :+ newX2, completed)
          case _ =>
            _solve(xs :+ x.copy(idx = x.idx + 1), completed)
        }
    }
    println("=" *50)
    if(dna.toSet.size == 4) {
      val ret = _solve(Vector(Accr(None, 0)), Vector()).filter((p) => p.a == p.c && p.b == p.d)
      //    ret.foreach(x => println(x.toString))
      //    println("=" *50)
      ret.length
    }else {
      0
    }
  }

  def main(args: Array[String]): Unit = {
    val INPUT = "D-small-practice.in"
    val OUTPUT = INPUT.takeWhile(_ != '.') + ".out"
    val isConsole = false

    val itr = Source.fromFile(INPUT).getLines()
    val stream = if (isConsole) Console.out else new PrintStream(OUTPUT)
    try {
      val sets = itr.next().toInt
      (1 to sets).foreach { set =>
        stream.println(f"Case #$set: ${solve(itr.next())}")
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}