import scala.io.Source
import com.typesafe.scalalogging.Logger

import scala.annotation.tailrec

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day20")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =

    //println(presents(100000, inputLines.head.toLong).last)
    //println(integer(1, inputLines.head.toLong).tapEach(println).take(15).last)
    //val firstPrimes = Primes.primes.take(10)

    val limit = inputLines.head.toLong

    for
      index <- 1000 to Int.MaxValue
    do
      val newValue = rangeLong(2, (index / 2)).foldLeft(1l + index):
        case (acc, elfNumber) => index % elfNumber == 0 match
          case true => acc + elfNumber * 1l
          case false => acc
      if (newValue * 10 >= limit)
        println(index)

    //(1 to 9).map(index => firstPrimes.tapEach(current => print(s"$current,")).take(index).product).foreach(println)
    //(1 to 9).map(index => presents(firstPrimes.take(index).product, 3400000).head).foreach(println)

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
end Solver



object Primes:
  val primes: LazyList[Int] = primes(1)
  def primes(from: Int, previous: List[Int] = Nil): LazyList[Int] =
    @tailrec
    def next(current: Int): Int =
      previous.filterNot(_ == 1).forall(current % _ != 0) match
        case true => current
        case false => next(current + 1)

    val nextVal = next(from)
    nextVal #:: primes(nextVal + 1, nextVal :: previous)


def factorial(toNumber: Long): Long =
  rangeLong(1l, toNumber).foldLeft(1l)(_ * _)

def integer(houseNumber: Int, limit: Long): LazyList[Long] =
  val newValue = (math.pow(houseNumber, 2) + houseNumber) / 2

  newValue >= limit match
    case true => LazyList.empty
    case false => LazyList.cons(newValue.toLong, integer(houseNumber + 1, limit))
def rangeLong(from: Long, to: Long): LazyList[Long] =
  from > to match
    case true => LazyList.empty
    case false => LazyList.cons(from, rangeLong(from + 1 , to))

def present(houseNumber: Long): Long =
  rangeLong(1, houseNumber).foldLeft(0l):
    case (acc, elfNumber) => houseNumber%elfNumber == 0 match
      case true => acc + elfNumber * 10l
      case false => acc


def presents(houseNumber: Long, limit: Long): LazyList[Long] =
  val newValue = rangeLong(1, houseNumber).foldLeft(0l):
    case (acc, elfNumber) => houseNumber%elfNumber == 0 match
      case true => acc + elfNumber * 10l
      case false => acc
  newValue >= limit match
    case true => houseNumber #:: LazyList.empty
    case false => LazyList.cons(houseNumber, presents(houseNumber + 1, limit))
