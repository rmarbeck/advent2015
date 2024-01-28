import scala.io.Source
import com.typesafe.scalalogging.Logger

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day2")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =

    val (resultPart1, resultPart2) = inputLines.map:
      case s"${height}x${length}x${width}" => new Package(height, length, width)
    .map(pack => (pack.paperNeeded, pack.ribbonNeeded))
    .unzip

    val result1 = s"${resultPart1.sum}"
    val result2 = s"${resultPart2.sum}"

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

case class Package(height: Int, length: Int, width: Int):
  def this(heightAsString: String, lengthAsString: String, widthAsString: String) =
    this(heightAsString.toInt, lengthAsString.toInt, widthAsString.toInt)
  private val vals: List[Int] = List(height, length, width).sorted
  private val mins: List[Int] = vals.take(2)
  def paperNeeded: Int =
    vals.combinations(2).map(2*_.product).sum + mins.product
  def ribbonNeeded: Int =
    vals.product + mins.map(2*_).sum