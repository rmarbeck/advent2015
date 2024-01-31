import scala.io.Source
import com.typesafe.scalalogging.Logger

import scala.annotation.tailrec

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day11")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =

    val resultPart1 = findNextValidPassword(inputLines.head)
    val resultPart2 = findNextValidPassword(resultPart1.password)


    val result1 = s"${resultPart1.password}"
    val result2 = s"${resultPart2.password}"

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

  def findNextValidPassword(from: String): Password =
    def passwordLazyList(from: String): LazyList[Password] =
      val newValue = Password(from).next
      newValue #:: passwordLazyList(newValue.password)
    passwordLazyList(from).find(_.isValid).get


  val forbidden = Array('i', 'o', 'l')
  val forbiddenAsInt = forbidden.map(_.toInt)
  val alphabet = ('a' to 'z').toArray

  val aAsInt = 'a'.toInt
  val zAsInt = 'z'.toInt

  case class Password(password: String):
    def isValid: Boolean = doesNotContainForbidden && containsThreeIncreasingLetter && containsTwoDifferentPairs

    def next: Password =
      @tailrec
      def findNext(current: String, toAdd: Int = 0): String =
        current.length match
          case 0 => "a" * toAdd
          case _ =>
            val (start, last) = (current.init, current.last)
            last match
              case 'z' => findNext(start, toAdd + 1)
              case value => start + alphabet(alphabet.indexOf(value) + 1) + "a" * toAdd

      Password(findNext(password))

    private def doesNotContainForbidden: Boolean = password.intersect(forbidden).isEmpty

    private def containsThreeIncreasingLetter: Boolean =
      def matches(chars: Array[Char]): Boolean =
        chars.map(alphabet.indexOf(_)).sliding(2, 1).forall(value => value(1) == value(0) + 1)

      password.sliding(3, 1).map(_.toCharArray).exists(matches)

    private def containsTwoDifferentPairs: Boolean = alphabet.map(_.toString * 2).count(password.contains(_)) >= 2

end Solver

