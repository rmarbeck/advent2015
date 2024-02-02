import scala.io.Source
import com.typesafe.scalalogging.Logger

import scala.annotation.tailrec

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day19")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =

    val (replacements, molecule) = inputLines.filterNot(_.isEmpty).foldLeft((List[Replacement](), "")):
      case (acc, newLine) =>
        newLine match
          case s"$from => $to" => (Replacement(from, to) :: acc(0), acc(1))
          case value => (acc(0), value)

    var results = Set[String]()

    replacements.map(replace => indexes(molecule, replace.from, Nil)).tapEach(println)


    /*replacements.foreach:
      case replacement => molecule.*/



    val result1 = s""
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

  @tailrec
  def indexes(toLookIn: String, toFind: String, indexList: List[Int]): List[Int] =
    toLookIn match
      case value if value.isEmpty => indexList
      case _ =>
        val currentHead = indexList.headOption.map(_ + toFind.length).getOrElse(0)
        toLookIn.indexOf(toFind, currentHead) match
          case -1 => indexList
          case value => indexes(toLookIn, toFind, (currentHead + value) :: indexList)

end Solver

case class Replacement(from: String, to: String)
