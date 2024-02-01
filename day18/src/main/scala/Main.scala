import scala.io.Source
import com.typesafe.scalalogging.Logger

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day18")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =

    val numberOfRounds = 100

    val screen = parseLines(inputLines)

    val (newScreenPart1, newScreenPart2) = (1 to numberOfRounds).foldLeft((screen, screen.turnOnCorners)):
      case (acc, _) => (acc(0).nextPart1, acc(1).nextPart2)

    val result1 = s"${newScreenPart1.numberOn}"
    val result2 = s"${newScreenPart2.numberOn}"

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

  def parseLines(inputLines: Seq[String]): Screen =
    val asArray =
      inputLines.map:
        case line => line.map:
            case char => char match
              case '#' => true
              case _ => false
          .toArray
      .toArray
    Screen(asArray)

end Solver

case class Screen(lights: Array[Array[Boolean]]):
  def turnOnCorners: Screen =
    val newLights = Array.tabulate(height, width):
      case (row, col) if row == 0 && col == 0 => true
      case (row, col) if row == 0 && col == width - 1 => true
      case (row, col) if row == height - 1 && col == 0 => true
      case (row, col) if row == height - 1 && col == width - 1 => true
      case (row, col) => lights(row)(col)
    Screen(newLights)
  lazy val height = lights.length
  lazy val width = lights(0).length
  def countNeightBoorsOf(row: Int, col: Int): Int =
    //val (rows, cols) = List(-1, 0, 1).map(incr => (row + incr, col + incr)).unzip
    //rows.flatMap(curRow => cols.map((curRow, _))).filterNot(_ == (row, col))
    //too slow
    List((row-1, col-1), (row-1, col), (row-1, col+1), (row, col-1), (row, col+1), (row+1, col-1), (row+1, col), (row+1, col+1)).map:
      case (currentRow, currentCol) => lights.isDefinedAt(currentRow) && lights(currentRow).isDefinedAt(currentCol) match
        case true => lights(currentRow)(currentCol)
        case _ => false
    .count(_ == true)
  def this(values: List[Boolean]) =
    this(values.grouped(math.sqrt(values.length).toInt).map(_.toArray).toArray)

  def nextPart1: Screen =
    val newLights = for
      row <- 0 until height
      col <- 0 until width
    yield
      (lights(row)(col), countNeightBoorsOf(row, col)) match
        case (_, 3) => true
        case (true, 2) => true
        case _ => false

    new Screen(newLights.toList)

  def nextPart2: Screen = nextPart1.turnOnCorners

  def numberOn: Int = lights.flatten.count(_ == true)