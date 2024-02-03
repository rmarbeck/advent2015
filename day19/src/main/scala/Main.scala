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

    val result = replacements.flatMap(replacement => replace(molecule, replacement.from, replacement.to)).toSet

    def reduceMolecule(moleculeToReduce: String): String =
      replacements.sortBy(_.to.size).reverse.foldLeft(moleculeToReduce):
        case (acc, replacement) if acc == moleculeToReduce => replaceAll(acc, replacement.to, replacement.from)
        case (acc, _) => acc

    replacements.map(_.to).foreach:
      case replacement => println(s"$replacement : ${indexes(molecule, replacement,Nil).length}")

    val resultPart2 = (0 to 0).foldLeft(Set("e")):
      case (acc, index) if acc.contains("HOH") => println(s"Found : ${index - 1}"); Set("")
      case (acc, _) => next(acc, replacements) -- acc



    val result1 = s"${result.size}"
    val result2 = s"${resultPart2}"

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

  def next(from: Set[String], replacements: List[Replacement]): Set[String] =
    from.flatMap:
      case currentMolecule => replacements.flatMap(replacement => replace(currentMolecule, replacement.from, replacement.to)).toSet

  def replaceAll(toReplaceIn: String, toFind: String, replacement: String): String =
    toReplaceIn.replaceFirst(toFind, replacement)

  def replace(toReplaceIn: String, toFind: String, replacement: String): Set[String] =
    val indexesWhereToReplace = indexes(toReplaceIn, toFind, Nil).distinct
    val (mainSize, patternSize) = (toReplaceIn.length, toFind.length)
    indexesWhereToReplace.map:
      case currentIndex =>
        val start = toReplaceIn.slice(0, currentIndex)
        val end = toReplaceIn.slice(currentIndex + patternSize, mainSize-1)
        start + replacement + end
    .toSet

  @tailrec
  def indexes(toLookIn: String, toFind: String, indexList: List[Int]): List[Int] =
    val currentHead = indexList.headOption.map(_ + toFind.length).getOrElse(0)
    toLookIn.indexOf(toFind, currentHead) match
      case -1 => indexList
      case value => indexes(toLookIn, toFind, value :: indexList)

end Solver

case class Replacement(from: String, to: String)
