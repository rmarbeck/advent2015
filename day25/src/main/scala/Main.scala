import scala.io.Source
import com.typesafe.scalalogging.Logger

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day25")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  val (veryFirst, multiplyBy, divideBy) = (20151125l, 252533l, 33554393l)
  def runOn(inputLines: Seq[String]): (String, String) =
    val (rowToFind, colToFind) = inputLines.head match
      case s"To continue, please consult the code grid in the manual.  Enter the code at row $row, column $col." => (row.toInt, col.toInt)

    val result = listOfValues(veryFirst.toInt).drop(valueInArrayFor(rowToFind, colToFind) - 1).head

    val result1 = s"$result"
    val result2 = s"Finished"

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

def valueInArrayFor(row: Int, col: Int): Int =
  val diag = (col + row - 1)
  ((diag + 1) * (diag / 2d)).toInt - (row - 1)

def listOfValues(current: Int): LazyList[Int] =
  val next = (current.toLong * Solver.multiplyBy) % Solver.divideBy
  current #:: listOfValues(next.toInt)