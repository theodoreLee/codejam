import java.io.PrintStream
import scala.io.Source

object NewCalendar {
  def gcd(m: Long, n: Long): Long = n match {
    case _ if m < n => gcd(n, m)
    case _ if n == 0 => m
    case _ => gcd(n, m % n)
  }

  def solve(monthsOfYear: Long, daysOfMonth: Long, daysOfWeek: Long) = {
    val rest = daysOfMonth % daysOfWeek
    val nextFullMonth = daysOfWeek / gcd(rest, daysOfWeek) // 한주일수 / gcd(나머지일수 한주일수

    def getLinesOfMonth(restMonths: Long, restOfBefore: Long, accr: Vector[Long]): Vector[Long] = restMonths match {
      case 0 => accr
      case _ =>
        //  Math.ceil(달의일수 + (이전달까지의 나머지의 합 % 한주의 일수)) / 한주의 일수)
        val weeks = Math.ceil((daysOfMonth.toDouble + restOfBefore) / daysOfWeek).toLong
        getLinesOfMonth(restMonths - 1, (restOfBefore + rest) % daysOfWeek, accr :+ weeks)
    }

    val weeks = getLinesOfMonth(nextFullMonth min monthsOfYear, 0, Vector())
    weeks.sum * (monthsOfYear / nextFullMonth) + weeks.take((monthsOfYear % nextFullMonth).toInt).sum
  }

  def main(args: Array[String]): Unit = {
    val INPUT = "A-large-practice.in"
    val OUTPUT = INPUT.takeWhile(_ != '.') + ".out"
    val isConsole = false

    val itr = Source.fromFile(INPUT).getLines()
    val stream = if (isConsole) Console.out else new PrintStream(OUTPUT)
    try {
      val sets = itr.next().toInt
      (1 to sets).foreach { set =>
        val Array(months, days, weeks) = itr.next().split(" ").map(_.toLong)
        stream.println(f"Case #$set: ${solve(months, days, weeks)}")
      }
    } finally {
      stream.flush()
      if (!isConsole) stream.close()
    }
  }
}


