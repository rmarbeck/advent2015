import scala.io.Source
import com.typesafe.scalalogging.Logger

import scala.annotation.tailrec

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day20")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =

    val limit = inputLines.head.toLong

    val softLimit = (limit / 10).toInt

    val housePart1 = Array.fill(softLimit)(1)
    val housePart2 = Array.fill(softLimit)(1)

    for
      index <- 2 until softLimit
      j <- index until softLimit by index
      if j % index == 0 && housePart1(j-1) < limit
    do
      housePart1(j) += index * 10
      if (j <= index * 50)
        housePart2(j) += index * 11

    val List(resultPart1, resultPart2) = List(housePart1, housePart2).map(_.zipWithIndex.filter(_._1 >= limit.toInt).map(_._2).sorted.head)

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