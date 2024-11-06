import scala.io.Source
import com.typesafe.scalalogging.Logger

import scala.annotation.tailrec

val loggerAOC: Logger = Logger("aoc")
val loggerAOCPart1: Logger = Logger("aoc.part1")
val loggerAOCPart2: Logger = Logger("aoc.part2")

@main def hello(): Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day8")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : $score1")
    println(s"2 : $score2")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =

    val List(standardLength, lessLength, moreLength) = inputLines.map(compute).unzip3.toList.map(_.sum)

    val result1 = s"${standardLength - lessLength}"
    val result2 = s"${moreLength - standardLength}"

    (s"$result1", s"$result2")

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

  private def compute(line: String): (Int, Int, Int) =
    val chars = line.toList
    (chars.length, less2(chars), more2(chars))

  @tailrec
  private def less2(rawString: List[Char], numberOfAntiSlash: Int = 0, current: Int = 0): Int =
    rawString match
      case Nil => current
      case head :: tail =>
        (numberOfAntiSlash, head) match
          case (0, '\\') => less2(tail, 1, current)
          case (0, '"') => less2(tail, 0, current)
          case (1, 'x') => less2(tail, 2, current)
          case (2, _) => less2(tail, 3, current)
          case _ => less2(tail, 0, current + 1)

  @tailrec
  private def more2(rawString: List[Char], numberOfAntiSlash: Int = 0, current: Int = 0): Int =
    rawString match
      case Nil => current
      case head :: tail =>
        (numberOfAntiSlash, head) match
          case (0, '\\') => more2(tail, 1, current)
          case (0, '"') | (1, 'x') => more2(tail, 0, current + 3)
          case (0, _) => more2(tail, 0, current + 1)
          case (1, '\\' | '"') => more2(tail, 0, current + 4)
          case _ => throw Exception("Not supported")

end Solver