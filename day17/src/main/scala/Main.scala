import scala.io.Source
import com.typesafe.scalalogging.Logger

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day17")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =

    val containers = inputLines.zipWithIndex.map:
      case (s"$capacity", index) => Container(index.toString, capacity.toInt)

    val containersNumber = containers.length

    val (resultForPart2, resultPart1) = (1 to containersNumber).map:
      case size => (size, containers.map(_.rank).combinations(size).count(_.map(containerRank => containers.find(_.rank == containerRank).get.capacity).sum == 150))
    .unzip

    val resultPart2 = resultPart1.zipWithIndex.find(_._1 != 0).map(_._2).map(resultForPart2(_)).getOrElse(0)

    val result1 = s"${resultPart1.sum}"
    val result2 = s"${resultPart2}"

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

case class Container(rank: String, capacity: Int)
