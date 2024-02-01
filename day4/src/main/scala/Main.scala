import scala.io.Source
import com.typesafe.scalalogging.Logger
import scala.collection.parallel.CollectionConverters.*


val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day4")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =

    val (resultPart1: Int, resultPart2: Int) =
      startsWith(6, inputLines.head).foldLeft((0, 0)):
        case (acc, value) => value == 5 match
          case true if acc(0) == 0 => (acc(1), acc(1) + 1)
          case _ => (acc(0), acc(1) + 1)

    val result1 = s"${resultPart1}"
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
end Solver

def startsWith(numberOfZeros: Int, key: String, index: Int = 0): LazyList[Int] =
  MD5.hashNumberOfLeadingZerosBasedOnFirstBytes(key + index.toString, numberOfZeros) match
    case value if value >= numberOfZeros => LazyList.empty
    case value => LazyList.cons(value, startsWith(numberOfZeros, key, index+1))


object MD5 {
  def hashNumberOfLeadingZerosBasedOnFirstBytes(s: String, maxZerosToFind: Int): Int = {
    val nbOfBytes = math.ceil(maxZerosToFind / 2d).toInt
    val m = java.security.MessageDigest.getInstance("MD5")
    m.update(s.getBytes)
    val firstBytes = m.digest.take(maxZerosToFind/2)
    val valueOfFirstBytes = firstBytes.foldLeft(0):
      case (acc, currentByte) => acc * 256 + currentByte.toInt

    valueOfFirstBytes match
      case 0 => maxZerosToFind
      case value if value > 0 && value < 16 => maxZerosToFind - 1
      case _ => -1
  }

  def hashNumberOfLeadingZeros(s: String): Int = {
    val m = java.security.MessageDigest.getInstance("MD5")
    m.update(s.getBytes)
    val valueAsBytes = m.digest
    32 - new java.math.BigInteger(1, valueAsBytes).toString(16).length
  }
  def hash(s: String): String = {
    val m = java.security.MessageDigest.getInstance("MD5")
    val b = s.getBytes("UTF-8")
    m.update(b, 0, b.length)
    val result = new java.math.BigInteger(1, m.digest()).toString(16)
    32-result.length match
      case 0 => result
      case value => "0"*value + result
  }
}