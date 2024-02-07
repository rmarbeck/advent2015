import scala.io.Source
import com.typesafe.scalalogging.Logger

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day24")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =

    val weights = inputLines.map(_.toInt)

    val resultPart1 = findEntanglement(weights, 3)
    val resultPart2 = findEntanglement(weights, 4)


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

  private def findEntanglement(listOfInts: Seq[Int], nbOfGroups: Int): Long =
    val target: Int = listOfInts.sum / nbOfGroups
    val lowestSize = findLowestGroupSize(listOfInts, nbOfGroups)
    listOfInts.combinations(lowestSize).filter(_.sum == target).map(_.map(_.toLong).product).min

  private def findLowestGroupSize(listOfInts: Seq[Int], nbOfGroups: Int): Int =
    val nbOfInts: Int = listOfInts.size
    val target: Int = listOfInts.sum / nbOfGroups
    val result =
      (2 to nbOfInts - 2).find:
        case sizeForFirstGroup => listOfInts.combinations(sizeForFirstGroup).exists(_.sum == target)
    result match
      case Some(value) => value
      case None => throw Exception("No result found")

end Solver