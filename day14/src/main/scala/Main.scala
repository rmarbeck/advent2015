import scala.io.Source
import com.typesafe.scalalogging.Logger

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day14")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =

    val totalTime = 2503

    val reindeers = inputLines.map:
      case s"$name can fly $speed km/s for $running seconds, but then must rest for $resting seconds." => Reindeer(name, speed.toInt, running.toInt, resting.toInt)

    val resultPart1 = reindeers.map(_.travelDistance(totalTime)).max

    val resultPart2 = (1 to totalTime).flatMap:
      case minute =>
        val resultOnThisMinute = reindeers.map:
            case reindeer => (reindeer, reindeer.travelDistance(minute))
        val bestDistance = resultOnThisMinute.map(_._2).max
        resultOnThisMinute.filter(_._2 == bestDistance).map(_._1)
    .groupMapReduce(identity)(_ => 1)(_ + _)
    .map(_._2).max

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

  def solveTest: (String, String) =
    solver("test.txt")
  def solve: (String, String) =
    solver("data.txt")
  private def solver(fileName: String): (String, String) =
    val bufferedSource = Source.fromResource(fileName)
    val lines = bufferedSource.getLines().toSeq
    lines match
      case Nil => ("", "")
      case _ => runOn(lines)
end Solver

case class Reindeer(name: String, speed: Int, running: Int, resting: Int):
  def travelDistance(minute: Int) =
    val (fullCycleNumber, fullCycleNumberModulo) = (minute / (running + resting),  minute % (running + resting))
    fullCycleNumber * running * speed + math.min(running, fullCycleNumberModulo) * speed
