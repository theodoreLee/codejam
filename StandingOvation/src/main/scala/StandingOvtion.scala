import java.io.FileOutputStream

import scala.io.Source

/**
 * Input
 *
 * The first line of the input gives the number of test cases, T. T test cases follow.
 * Each consists of one line with Smax, the maximum shyness level of the shyest person in the audience, followed by a string of Smax + 1 single digits.
 * The kth digit of this string (counting starting from 0) represents how many people in the audience have shyness level k.
 * For example, the string "409" would mean that there were four audience members with Si = 0 and nine audience members with Si = 2
 * (and none with Si = 1 or any other value).
 *
 * Note that there will initially always be between 0 and 9 people with each shyness level.
 *
 * The string will never end in a 0. Note that this implies that there will always be at least one person in the audience.
 *
 * Output
 *
 * For each test case, output one line containing "Case #x: y",
 * where x is the test case number (starting from 1) and y is the minimum number of friends you must invite.
 */
object StandingOvtion {
  val INPUT = "A-large.in"
  val OUTPUT = INPUT.takeWhile(_ != '.') + ".out"


  def main(args: Array[String]): Unit = {
    val itr = Source.fromFile(INPUT).getLines()
    val sets = itr.next().toInt

    val writer = new FileOutputStream(OUTPUT)
//        val writer = Console.out
    try {
      Console.withOut(writer) {
        for (set <- 1 to sets) {
          val Array(_, listString) = itr.next().split(' ')
          println(s"Case #${set}: ${numberOfFriends(listString.toList.map(_.toString.toInt))}")
        }
      }
    } finally {
      writer.flush()
      writer.close()
    }
  }

  def numberOfFriends(list:List[Int], shynessLevel:Int = 0, total:Int = 0, friends:Int = 0):Int = list match {
    case Nil => friends
    case x :: xs if shynessLevel <= total => numberOfFriends(xs, shynessLevel + 1, total + x, friends)
    case x :: xs =>
      val needMoreFriends = shynessLevel - total
      numberOfFriends(xs, shynessLevel + 1, total + x + needMoreFriends, friends + needMoreFriends)
  }
}