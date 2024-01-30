import scala.io.Source
import com.typesafe.scalalogging.Logger

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

    val litteralsSum = inputLines.map(_.length).sum

    def compute(value: String) =
      val result = value.replace("\\\\","_").toCharArray.sliding(2, 1).foldLeft((0, 6)):
        case (acc, Array('\\', 'x')) => (acc(0) - 2, acc(1) + 2)
        case (acc, Array('\\', '"')) => (acc(0) + 1, acc(1) + 2)
        case (acc, Array('\\', '\\')) => (acc(0), acc(1) + 2)
        case (acc, Array('_', _)) => (acc(0), acc(1) + 4)
        case (acc, Array('"', '"')) => (acc(0), acc(1))
        case (acc, Array('"', _)) => (acc(0), acc(1) + 1)
        case (acc, Array(_, '"')) => (acc(0) + 1, acc(1) + 1)
        case (acc, _) => (acc(0) + 1, acc(1) + 1)
      println(s"$value [${value.replace("\\\\","_")}] => $result [${value.length}]")
      result

    val inMemorySum = inputLines.map(compute(_)._1).sum
    val inMemorySum2 = inputLines.map(compute(_)._2).sum

    println(s"$litteralsSum")
    println(s"$inMemorySum")
    println(s"$inMemorySum2")

    val result1 = s"${litteralsSum - inMemorySum}"
    val result2 = s""

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
