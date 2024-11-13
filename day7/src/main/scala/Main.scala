import scala.io.Source
import com.typesafe.scalalogging.Logger

val loggerAOC: Logger = Logger("aoc")
val loggerAOCPart1: Logger = Logger("aoc.part1")
val loggerAOCPart2: Logger = Logger("aoc.part2")

/**
 *
 * #Interesting
 *
 * Using function litteral and implicit (context param) through use of ?=> syntax
 *
 * Even if function is typed to need implicit, a function without implicit in signature type checks (use for result1)
 *
 */
@main def hello(): Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day7")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : $score1")
    println(s"2 : $score2")
    println(s"----------------")
  println("Done")

object Solver:
  private def runOn(inputLines: Seq[String]): (String, String) =

    val populateFromInput = populateAndRead(inputLines)

    val result1 = populateFromInput(())

    val result2 = populateFromInput:
      summon[WireBox].addOrChange(Simple("b", result1.toString))

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

def populateAndRead(inputLines: Seq[String])(updateWiresFunction: WireBox ?=> Unit): Int =
  given box: WireBox = new WireBox()
  populateBox(inputLines)
  updateWiresFunction.apply
  box.getWire("a").unSignedValue

def populateBox(inputLines: Seq[String])(using WireBox): Unit =
  inputLines.map:
    case s"$left AND $right -> $name" => BitWiseAnd(name, left, right)
    case s"$left OR $right -> $name" => BitWiseOr(name, left, right)
    case s"$left LSHIFT $right -> $name" => LeftShift(name, left, right.toInt)
    case s"$left RSHIFT $right -> $name" => RightShift(name, left, right.toInt)
    case s"NOT $right -> $name" => NOT(name, right)
    case s"$value -> $name" => Simple(name, value)
  .foreach(summon[WireBox].addOrChange)

class WireBox:
  private var wireList: Map[String, Wire] = Map()

  def addOrChange(wire: Wire): Unit = wireList = wireList + (wire.name -> wire)

  def getWire(name: String): Wire = wireList.get(name) match
    case Some(value) => value
    case None => throw Exception(s"Not found $name")

trait Wire(val name: String):
  def unSignedValue(using WireBox): Int = value
  def value: Int
  def getValue(nameOrValue: String)(using wirebox: WireBox): Int =
    nameOrValue.toIntOption match
      case Some(valueAsInt) => valueAsInt
      case None => wirebox.getWire(nameOrValue).value

case class Simple(simpleName: String, other: String)(using WireBox) extends Wire(simpleName):
  lazy val value: Int = getValue(other)

case class BitWiseAnd(bitWiseAndName: String, left: String, right: String)(using WireBox) extends Wire(bitWiseAndName):
  lazy val value: Int = getValue(left) & getValue(right)

case class BitWiseOr(bitWiseOrName: String, left: String, right: String)(using WireBox) extends Wire(bitWiseOrName):
  lazy val value: Int = getValue(left) | getValue(right)

case class LeftShift(leftShiftName: String, left: String, right: Int)(using WireBox) extends Wire(leftShiftName):
  lazy val value: Int = getValue(left) << right

case class RightShift(rightShiftName: String, left: String, right: Int)(using WireBox) extends Wire(rightShiftName):
  lazy val value: Int = getValue(left) >> right

case class NOT(notName: String, right: String)(using WireBox) extends Wire(notName):
  lazy val value: Int = ~ getValue(right)