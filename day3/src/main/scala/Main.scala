import scala.io.Source
import com.typesafe.scalalogging.Logger

val loggerAOC: Logger = Logger("aoc")
val loggerAOCPart1: Logger = Logger("aoc.part1")
val loggerAOCPart2: Logger = Logger("aoc.part2")

@main def hello(): Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day3")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : $score1")
    println(s"2 : $score2")
    println(s"----------------")
  println("Done")

object Solver:
  private def runOn(inputLines: Seq[String]): (String, String) =

    val input: String = inputLines.head

    val resultPart1 = calculate(input)

    val List(odd, even) = input.zipWithIndex.partitionMap:
      case (value, index) if index % 2 == 0 => Left(value)
      case (value, _) => Right(value)
    .toList.map(_.mkString)

    val resultPart2 = calculate(odd, even)

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

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
end Solver

def calculate(values: String*): Int =
  def calculate(directions: String): Set[Position] =
    directions.map(Direction.from).foldLeft(Path.start)(_ trace _).positions
  values.map(calculate).reduce(_ ++ _).size

case class Path(positions: Set[Position], current: Position):
  def trace(direction: Direction): Path =
    val newPosition = current.move(direction)
    Path(positions + newPosition, newPosition)

object Path:
  import Position.*
  lazy val start = new Path(Set(origin), origin)

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

object Position:
  lazy val origin: Position = Position(0, 0)
