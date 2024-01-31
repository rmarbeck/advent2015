import scala.io.Source
import com.typesafe.scalalogging.Logger

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day12")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =

    val resultPart1 =
      inputLines.map:
        case char => char.foldLeft((List[String](), false)):
          case ((acc, true), newChar) =>
            newChar.isDigit match
              case true => ((acc.head :+ newChar) :: acc.tail, true)
              case false => (acc, false)
          case ((acc, false), newChar) =>
            newChar.isDigit || newChar == '-' match
              case true =>  (newChar.toString :: acc, true)
              case false => (acc, false)
        ._1.map(_.toInt).sum
      .sum

    val resultPart2 = inputLines.map(ujson.read(_)).map:
      case json: ujson.Value => traverse(json).map(_.toInt).sum
    .sum

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

  def traverse(v: ujson.Value): Iterable[String] =
    v match {
    case a: ujson.Arr => a.arr.flatMap(traverse)
    case o: ujson.Obj =>
      o.obj.values.exists(_.value.toString == "red") match
        case true => Nil
        case false => o.obj.values.flatMap(traverse)
    case n: ujson.Num => Seq(n.toString)
    case _ => Nil
  }

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
