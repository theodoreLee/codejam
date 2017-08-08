object HikingDeer2 extends App {

  def minEncounter(hikers: Seq[(Int, Int, Int)]): Int = {

    val h = hikers.map(_._2).sum

    def loop(events: collection.SortedMap[Double, Set[(Int, Int)]], current: Int, min: Int): Int = {

      //      println(events, current, min)

      if (min == 0 || current >= 2 * h) min
      else {
        val (time, hikers): (Double, Set[(Int, Int)]) = events.head

        val d = (for {
          (speed, count) <- hikers
          d = if (count == 0) -1 else 1
        } yield d).sum

        val nextEvents = (events.tail /: hikers) {
          case (events, (speed, count)) =>
            events + ((time + speed) -> (events.getOrElse(time + speed, Set.empty) + ((speed, count + 1))))
        }

        loop(nextEvents, current + d, min min (current + d))
      }
    }

    val events = for {
      (position, numberOfHikers, initSpeed) <- hikers
      i <- 0 until numberOfHikers
      speed = i + initSpeed
    } yield ((360 - position).toDouble / 360 * speed, speed)

    val eventMap = collection.SortedMap.empty[Double, Set[(Int, Int)]] ++ events.groupBy(_._1).mapValues {
      case events: Seq[(Double, Int)] => events.map(x => (x._2, 0)).toSet
    }

    loop(eventMap, h, h)
  }

  def process(lineIn: Iterator[String])(lineOut: String => Unit) =
    for (i <- 1 to lineIn.next().toInt) {
      val hikers = Seq.fill(lineIn.next().toInt) {
        val Array(a, b, c) = lineIn.next() split ' ' map (_.toInt)
        (a, b, c)
      }
      lineOut(s"Case #$i: ${minEncounter(hikers)}")
    }

  val filename = "C-small-practice-1"
  val writer = new java.io.PrintWriter(filename + ".out")
  try {
    process(io.Source.fromFile(filename + ".in").getLines) { s =>
      writer.println(s); writer.flush()
    }
  } finally {
    writer.flush(); writer.close()
  }

}