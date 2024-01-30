import scala.io.Source
import com.typesafe.scalalogging.Logger

import scala.annotation.tailrec

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day8")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =

    val results = inputLines.map(current => (current.length, less(current), more(current))).unzip3.toList.map(_.sum)

    val List(standardLength, lessLength, moreLength) = inputLines.map(current => (current.length, less(current), more(current))).unzip3.toList.map(_.sum)

    val result1 = s"${standardLength - lessLength}"
    val result2 = s"${moreLength - standardLength}"

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

  @tailrec
  def less(rawString: String, numberOfAntiSlash: Int = 0, current: Int = 0): Int =
    rawString.length match
      case 0 => current
      case _ =>
        val (head, tail) = (rawString.head, rawString.tail)
        numberOfAntiSlash match
          case 0 =>
            head match
              case '\\' => less(tail, 1, current)
              case '"' => less(tail, 0, current)
              case _ => less(tail, 0, current + 1)
          case 1 =>
            head match
              case '\\' => less(tail, 0, current + 1)
              case '"' => less(tail, 0, current + 1)
              case 'x' => less(tail, 2, current)
              case _ => less(tail, 0, current + 1)
          case 2 => less(tail, 3, current)
          case 3 => less(tail, 0, current + 1)

  @tailrec
  def more(rawString: String, numberOfAntiSlash: Int = 0, current: Int = 0): Int =
    rawString.length match
      case 0 => current
      case _ =>
        val (head, tail) = (rawString.head, rawString.tail)
        numberOfAntiSlash match
          case 0 =>
            head match
              case '\\' => more(tail, 1, current)
              case '"' => more(tail, 0, current + 3)
              case _ => more(tail, 0, current + 1)
          case 1 =>
            head match
              case '\\' => more(tail, 0, current + 4)
              case '"' => more(tail, 0, current + 4)
              case 'x' => more(tail, 0, current + 3)
              case _ => throw Exception("Not suppported")

end Solver


