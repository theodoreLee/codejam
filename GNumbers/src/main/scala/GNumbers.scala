import java.io.PrintStream
import scala.io.Source

object GNumbers {

  val primes = {
    // 1
    val MAX = Math.sqrt(("1"+"0"*15).toLong).toLong
    val MAX2 = Math.sqrt(MAX).toLong


    def _primes(max:Long, current:Long, set:Set[Long] = Set(2,3)):Set[Long] = {
      if(current > max) set
      else if(set.forall(current % _ != 0)) {
        _primes(max, current + 1, set + current)
      } else _primes(max, current + 1, set)
    }

    val minimalPrime = _primes(MAX2, 4)
    println("mini")
    def _primes2(max:Long, current:Long, accr:Set[Long]):Set[Long] = {
      if(current > max) accr ++ minimalPrime
      else if(minimalPrime.forall(current % _ != 0)) {
        _primes2(max, current + 1, accr + current)
      } else _primes2(max, current + 1, accr)
    }
    val ret = _primes2(MAX, MAX2 + 1, Set())
    println("ret")
    _primes(MAX, 4).intersect(ret)
  }

  println("primes",primes.mkString(","))

  def isPrime(originNumber: Long):Boolean = {
    val number = originNumber.toString.map(_.toString.toInt).sum
    number == 1 || (2 to Math.sqrt(number).toInt).forall( number % _ != 0)
  }


  def primeFactors(number: Int, max:Int, current: Int, ex: Int, vector: Vector[(Int, Int)]): Vector[(Int, Int)] = {
    if (number == 1) {
      if (ex == 0) vector else vector :+ ((current, ex))
    }
    else if(max < current) vector :+ ((number, 1))
    else if (number % current == 0) primeFactors(number / current, Math.sqrt(number / current).toInt, current, ex + 1, vector)
    else if (ex == 0) primeFactors(number, max, current + 1, ex, vector)
    else primeFactors(number, max, current + 1, 0, vector :+ ((current, ex)))
  }

  def solve(str: String) = {
    val originNumber = str.toLong


//    def doPlay(originNumber:Long, isLaurenceWin:Boolean) = {
//      if(isPrime(originNumber)) !isLaurenceWin
//      else
//    }
//    val gNumber = str.map(_.toString.toInt).sum
//    val originNumberPrimeFactors = primeFactors(originNumber, Math.sqrt(originNumber).toInt, 2, 0, Vector.empty).sortBy(_._2).reverse
//
//    def doPlay(rest:Int, factors:Vector[(Int,Int)], isLaurenceWin:Boolean) = factors match {
//      case Vector() => !isLaurenceWin
//      case x +: Vector() => if(x._2 == 1) !isLaurenceWin else isLaurenceWin
//      case _ =>
//        val ret = for {
//          (x, e) <- factors
//        } yield {
//          rest / (x * e)
//        }
//        val (nonePrime, prime) = ret.partition(x => isPrime(x.toString))
//    }

//    val result = if (gNumber == 1) true
//    else {
//      val ret = primeFactors(gNumber, Math.sqrt(gNumber).toInt, 2, 0, Vector.empty).sortBy(_._2).reverse
//      val s = if (ret.last._2 == 1) 1 else 0
//      (ret.length - s) % 2 == 0
//    }
//
//    val r = result match {
//      case true => "Seymour"
//      case false => "Laurence"
//    }
//    println(r)

//    r
    1
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
        stream.println(f"Case #$set: ${solve(itr.next())}")
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}