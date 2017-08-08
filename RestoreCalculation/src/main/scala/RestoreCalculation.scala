import java.io.PrintStream
import scala.io.Source

object RestoreCalculation {
  def getOrLess (number:Int, less:Int = 0) = {
    if(number == 15) {
      less
    } else number
  }

  //덧셈일때는 좌항의 값이 answer보다 작으면, carry 필요.
  def needCarry (left:Int, right:Int, answer:Int) = {
    getOrLess(left,9) + getOrLess(right, 9) < getOrLess(answer,0)
  }

  //left와 right 둘 다 값이 없을때는 right가 큰값이어야 함
  def findAnswer(left:Int, right:Int, answer:Int) = {
    if(left == 15 && right == 15) {
      (answer - 9, 9)
    } else if(left == 15) {
      (answer - right, right)
    } else if(right == 15) {
      (left, answer - right)
    } else {
      (left, right)
    }
  }
  def plus (left:String, right:String, answer:String) = {
    def _plus (left:Int, right:Int, answer:Int, carry:Int) = {
      if(left != 15 && right != 15) {
        (left, right, left + right + carry)
      } else {
        val _carry = needCarry( /*next left*/???, ???/*nextRight*/, ???/*nextAnswer*/)
        findAnswer(left, right, getOrLess(answer, carry) + (if(_carry) 10 else 0) )
      }
    }

    (left.toList.reverse, right.toList.reverse, answer.toList.reverse) match {
      case (Nil, Nil, Nil) =>
    }
  }

  def minus (left:String, right:String, answer:String) = {
    left + " - " + right +" = " + answer
  }

  def solve(left:String, oper:String, right:String, answer:String) = {
    println('?'.toInt - 48, '0'.toInt - 48, '1'.toInt - 48, '9'.toInt - 48)
    oper match {
      case "+" => plus(left, right, answer)
      case "-" => minus(left, right, answer)
    }
  }

  def main(args: Array[String]): Unit = {
    val INPUT = "test.in"
    val OUTPUT = INPUT.takeWhile(_ != '.') + ".out"
    val isConsole = false

    val itr = Source.fromFile(INPUT).getLines()
    val stream = if (isConsole) Console.out else new PrintStream(OUTPUT)
    try {
      val sets = itr.next().toInt
      (1 to sets).foreach { set =>
        val Array(left, oper, right, equal , answer ) = itr.next().split(' ')
        stream.println(f"Case #$set: ${solve(left, oper, right, answer)}")
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}