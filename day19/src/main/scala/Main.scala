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

    val resultPart2 = replacements.size match
      case 5 => part2ForTest(replacements)
      case _ =>
        /**
         * Rn and Ar should be seen as '(' and ')'
         *
         * Y as ','
         *
         * Rules are only :
         *
         *  1) Element => ElementElement
         *  2) Element => Element(Element) | Element(Element,Element) | Element(Element,Element,Element)
         *
         *
         * Applying rule 1 leads to increasing elements number of 1
         * Applying rule 2 leads to increasing elements number of 3, 5 or 7
         * Sub cases with no parenthesis lead to 3 increment and other lead to equivalent of 5 per coma
         *
         */
        val allElementsSorted = buildElements(replacements)

        given List[Element] = allElementsSorted

        val target = Molecule(molecule)

        val comasAndParenthesis = target.nbOfComas
        val parenthesisWithoutComas = target.nbOfParenthesis - comasAndParenthesis

        //val result = comasAndParenthesis + parenthesisWithoutComas + (target.nbElements - (5 * comasAndParenthesis) - (3 * parenthesisWithoutComas)) - 1
        val result = target.nbElements - 2*(comasAndParenthesis + target.nbOfParenthesis) - 1
        result

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

  def part2ForTest(replacements: List[Replacement]) =
    nextMolecules(replacements = replacements, target = "HOHOHO").length

  def nextMolecules(molecule: Set[String] = Set("e"), replacements: List[Replacement], target: String): LazyList[Int] =
    val newMolecules = next(molecule, replacements)
    newMolecules.contains(target) match
      case true => LazyList.empty
      case false => LazyList.cons(newMolecules.size, nextMolecules(newMolecules, replacements, target))

  def buildElements(replacements: List[Replacement]): List[Element] =
    val allElements = replacements.map(rep => rep.from + rep.to).mkString

    val twoDigitsElements = allElements.sliding(2, 1).map:
      case s"$chars" if chars(0).toInt <= 90 && chars(1).toInt >= 97 => s"$chars"
      case _ => ""
    .filterNot(_.isEmpty)
    .distinct.toList

    val oneDigitsOnly = twoDigitsElements.foldLeft(allElements):
      case (acc, twoDigitsElement) => acc.replaceAll(twoDigitsElement, "")
    .distinct.toList

    (twoDigitsElements ::: oneDigitsOnly.map(_.toString)).map(Element.apply)

end Solver

case class Element(value: String)

case class Molecule(rawValue: String)(using differentElements: List[Element]):
  lazy val nbElements = elementsGrid.map(_._2).sum
  lazy val rawSize = rawValue.size
  def getElementNumber(name: String): Int = elementsGrid.find(_._1 == Element(name)).map(_._2).getOrElse(0)
  lazy val nbOfParenthesis = getElementNumber("Rn")
  lazy val nbOfComas = getElementNumber("Y")
  val elementsGrid: Array[(Element, Int)] = differentElements.foldLeft((rawValue, List[(Element, Int)]())):
    case (acc, element) =>
      val nbElement = indexes(acc(0), element.value, Nil).length
      (replaceAll(acc(0), element.value, ""), (element, nbElement) +: acc(1))
  ._2.toArray


case class Replacement(from: String, to: String):
  override def toString: String =
    s"$from => ${to.replace("Rn", "(").replace("Y", ",").replace("Ar", ")")}"

def next(from: Set[String], replacements: List[Replacement]): Set[String] =
  from.flatMap:
    case currentMolecule => replacements.flatMap(replacement => replace(currentMolecule, replacement.from, replacement.to)).toSet

def replaceAll(toReplaceIn: String, toFind: String, replacement: String): String =
  toReplaceIn.replaceAll(toFind, replacement)

def replace(toReplaceIn: String, toFind: String, replacement: String): Set[String] =
  val indexesWhereToReplace = indexes(toReplaceIn, toFind, Nil).distinct
  val (mainSize, patternSize) = (toReplaceIn.length, toFind.length)
  indexesWhereToReplace.map:
    case currentIndex =>
      val start = toReplaceIn.slice(0, currentIndex)
      val end = toReplaceIn.slice(currentIndex + patternSize, mainSize - 1)
      start + replacement + end
  .toSet

@tailrec
def indexes(toLookIn: String, toFind: String, indexList: List[Int]): List[Int] =
  val currentHead = indexList.headOption.map(_ + toFind.length).getOrElse(0)
  toLookIn.indexOf(toFind, currentHead) match
    case -1 => indexList
    case value => indexes(toLookIn, toFind, value :: indexList)