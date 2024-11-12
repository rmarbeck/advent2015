import scala.io.Source
import com.typesafe.scalalogging.Logger

import scala.annotation.tailrec

val loggerAOC: Logger = Logger("aoc")
val loggerAOCPart1: Logger = Logger("aoc.part1")
val loggerAOCPart2: Logger = Logger("aoc.part2")

@main def hello(): Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day5")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : $score1")
    println(s"2 : $score2")
    println(s"----------------")
  println("Done")

object Solver:
  private def runOn(inputLines: Seq[String]): (String, String) =

    val List(resultPart1, resultPart2) = inputLines.map(NiceString.apply).map(current => (current.isNicePart1, current.isNicePart2)).unzip.toList.map(_.count(identity))

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

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

val vowels: Seq[Char] = List('a', 'e', 'i', 'o', 'u')
val forbidden: Seq[String] = List("ab", "cd", "pq", "xy")

case class NiceString(input: String):
  private def contains3Vowels: Boolean = input.count(vowels.contains) >= 3
  private def containsDouble: Boolean = ('a' to 'z').map(_.toString*2).exists(input.contains)
  private def doesNotContainForbidden: Boolean = ! forbidden.exists(input.contains)

  private def containsAPairTwice: Boolean =
    @tailrec
    def findPair(inString: String): Boolean =
      inString match
        case value if value.length < 4 => false
        case value =>
          val (start, pseudoTail) = value.splitAt(2)
          if pseudoTail.contains(start) then
            true
          else
            findPair(inString.tail)
    findPair(input)
  private def containsOneLetterSeparatedByOne: Boolean =
    @tailrec
    def findLetter(inString: String): Boolean =
      inString match
        case value if value.length < 3 => false
        case value if value(0) == value(2) => true
        case _ => findLetter(inString.tail)
    findLetter(input)

  def isNicePart1: Boolean = contains3Vowels && containsDouble && doesNotContainForbidden

  def isNicePart2: Boolean = containsAPairTwice && containsOneLetterSeparatedByOne
