import scala.io.Source
import com.typesafe.scalalogging.Logger

import scala.annotation.tailrec

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day10")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =

    val part1Limit = 40
    val part2Limit = 50
    val input = inputLines.head

    // incredibly faster than using string versions
    val result = nTimesUsingArray(input, part2Limit, part1Limit)

    //val result = nTimes(input, part2Limit, part1Limit)

    val result1 = s"${result(1)}"
    val result2 = s"${result(0)}"

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

  def nTimes(from: String, hardLimit: Int, softLimit: Int): (Int, Int) =
    val (hardResult, lengthSoft) = (1 to hardLimit).foldLeft((from, 0)):
      case (acc, index) =>
        val result = calc(acc(0))
        index == softLimit match
          case true => (result, result.size)
          case false => (result, acc(1))
    (hardResult.size, lengthSoft)

  def nTimesUsingArray(from: String, hardLimit: Int, softLimit: Int): (Int, Int) =
    val input = Array[Int](from.length) ++ from.toCharArray.map(_.asDigit)
    val (hardResult, lengthSoft) = (1 to hardLimit).foldLeft((input, 0)):
      case (acc, index) =>
        val result = roundWithArray(acc(0))
        index == softLimit match
          case true => (result, result(0))
          case false => (result, acc(1))
    (hardResult(0), lengthSoft)

  @tailrec
  def round(digits: StringBuilder, last: Char = ' ', countLast: Int = 0, result: StringBuilder = StringBuilder()): String =
    digits.isEmpty match
      case true => countLast match
        case 0 => result.toString
        case value => result.append(value.toString + last).toString
      case false =>
        val (head, tail) = (digits.head, digits.tail)
        countLast match
          case 0 => round(tail, head, 1, result)
          case value =>
            last == head match
              case true => round(tail, last, countLast + 1, result)
              case false => round(tail, head, 1, result.append(countLast.toString+last))

  def calc(digits: String): String =
    roundExp(StringBuilder(digits), StringBuilder(digits.length*2))

  @tailrec
  def roundExp(digits: StringBuilder, result: StringBuilder): String =
    val head = digits.head
    digits.indexWhere(_ != head) match
      case -1 => result.append(digits.size.toString+head).toString
      case value => roundExp(digits.drop(value), result.append(value.toString+head))


  def roundWithArray(digits: Array[Int]): Array[Int] =
    val length = digits(0)
    val result = Array.ofDim[Int](length * 2 + 1)
    val work = Array(digits(1), 1, 1)
    for
      index <- 2 to length by 1
    do
      digits(index) == work(0) match
        case true => work(1) = work(1) + 1
        case false =>
          result(work(2)) = work(1)
          result(work(2)+1) = work(0)
          work(0) = digits(index)
          work(1) = 1
          work(2) = work(2) + 2
    result(work(2)) = work(1)
    result(work(2) + 1) = work(0)
    result(0) = work(2) + 1
    result



end Solver
