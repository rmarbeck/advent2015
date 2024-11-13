import scala.io.Source
import com.typesafe.scalalogging.Logger

import scala.annotation.targetName

val loggerAOC: Logger = Logger("aoc")
val loggerAOCPart1: Logger = Logger("aoc.part1")
val loggerAOCPart2: Logger = Logger("aoc.part2")

@main def hello(): Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day1")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : $score1")
    println(s"2 : $score2")
    println(s"----------------")
  println("Done")

object Solver:
  private def runOn(inputLines: Seq[String]): (String, String) =

    /*val result = inputLines.head.scanLeft(0):
      case (acc, '(') => acc + 1
      case (acc, _) => acc - 1

    val result1 = s"${result.last}"
    val result2 = s"${result.indexWhere(_ == -1)}"*/

    /**
     * Solution avoiding passing through all elements (or a great number of elements) of resulting list
     */

    import scala.language.postfixOps
    val result = inputLines.head.foldLeft(Counter.Empty):
      case (counter, ')') => counter --
      case (counter, '(') => counter ++
      case (counter, unknownChar) => throw Exception(s"Not recognized character : $unknownChar")

    val result1 = s"${result.level}"
    val result2 = s"${result.part2Index}"

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

case class Counter(level: Int, unmatchedIndex: Option[Int], index: Int):
  lazy val part2Index: Int = unmatchedIndex.getOrElse(-1)
  @targetName("plusOne")
  def ++ : Counter = this.copy(level + 1, index = index + 1)
  @targetName("minusOne")
  def -- : Counter = level - 1 match
    case -1 if unmatchedIndex.isEmpty => this.copy(-1, Some(index), index = index + 1)
    case _ => this.copy(level - 1, index = index + 1)

object Counter:
  val Empty: Counter = Counter(0, None, 1)