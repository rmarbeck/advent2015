import Direction.North

import scala.io.Source
import com.typesafe.scalalogging.Logger

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day3")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =

    val resultPart1 = calc(inputLines.head)

    val (odd, even) = inputLines.head.grouped(2).foldLeft(("", "")):
      case (acc, duo) => (acc._1 + duo.head, acc._2 + duo.last)

    val resultPart2 = calc(odd, even)

    /*
    val alternative = inputLines.head.grouped(2).foldLeft(("", "")):
      case (acc, duo) => (acc._1 + duo.head, acc._2 + duo.last)

    val alternative: List[String] = test.toList

    val resultPart2 = calc(alternative: _*)*/

    val result1 = s"${resultPart1.size}"
    val result2 = s"${resultPart2.size}"

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

def calc(values: String*): Set[Position] =
  def calc(value: String): Set[Position] =
    value.map(Direction.from).foldLeft((Set(Position(0,0)), Position(0,0))):
      case (acc, newDirection) =>
        val newPosition = acc._2.move(newDirection)
        (acc._1 + newPosition, newPosition)
    ._1
  values.map(calc).reduce(_ ++ _)

enum Direction:
  case North, East, South, West

object Direction:
  def from(char: Char): Direction =
    char match
      case '^' => North
      case '>' => East
      case 'v' => South
      case '<' => West
      case _ => throw Exception("Not supported")

export Direction.*

case class Position(row: Int, col: Int):
  def move(direction: Direction): Position =
    direction match
      case North => this.copy(row = row - 1)
      case South => this.copy(row = row + 1)
      case West => this.copy(col = col - 1)
      case East => this.copy(col = col + 1)