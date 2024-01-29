import scala.io.Source
import com.typesafe.scalalogging.Logger

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day7")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =

    val box = WireBox()

    val wires, wires3 = getWires(inputLines)(using box)

    box.addWires(wires)
    val resultPart1 = box.getWire("a").unSignedValue(using box)

    val box2 = WireBox()
    val wires2 = getWires(inputLines)(using box2)

    val amendedWires = wires2.map:
      case Simple(name, _) if name == "b" => Simple(name, resultPart1.toString)(using box2)
      case value => value

    box2.addWires(amendedWires)

    val resultPart2 = box2.getWire("a").unSignedValue(using box2)

    val result1 = s"${resultPart1}"

    val result2 = s"$resultPart2"

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

def getWires(inputLines: Seq[String])(using WireBox) =
  inputLines.map:
    case s"$left AND $right -> $name" => BitWiseAnd(name, left, right)
    case s"$left OR $right -> $name" => BitWiseOr(name, left, right)
    case s"$left LSHIFT $right -> $name" => LeftShift(name, left, right.toInt)
    case s"$left RSHIFT $right -> $name" => RightShift(name, left, right.toInt)
    case s"NOT $right -> $name" => NOT(name, right)
    case s"$value -> $name" => Simple(name, value)

class WireBox:
  var wireList: Map[String, Wire] = Map()

  def addWires(wires: Seq[Wire]) =
    wireList = wireList ++ wires.map(current => current.name -> current).toMap

  def getWire(name: String): Wire = wireList.get(name) match
    case Some(value) => value
    case None => throw Exception(s"Not found ${name}")

trait Wire(val name: String):
  def unSignedValue(using WireBox): Int = value
  def value: Int
  def getValue(nameOrValue: String)(using wirebox: WireBox): Int =
    val result = nameOrValue.toIntOption match
      case Some(valueAsInt) => valueAsInt
      case None => wirebox.getWire(nameOrValue).value
    result

case class Simple(simpleName: String, other: String)(using WireBox) extends Wire(simpleName):
  lazy val value = getValue(other)

case class BitWiseAnd(bitWiseAndName: String, left: String, right: String)(using WireBox) extends Wire(bitWiseAndName):
  lazy val value = getValue(left) & getValue(right)

case class BitWiseOr(bitWiseOrName: String, left: String, right: String)(using WireBox) extends Wire(bitWiseOrName):
  lazy val value = getValue(left) | getValue(right)

case class LeftShift(leftShiftName: String, left: String, right: Int)(using WireBox) extends Wire(leftShiftName):
  lazy val value = getValue(left) << right

case class RightShift(rightShiftName: String, left: String, right: Int)(using WireBox) extends Wire(rightShiftName):
  lazy val value = getValue(left) >> right

case class NOT(notName: String, right: String)(using WireBox) extends Wire(notName):
  lazy val value = ~getValue(right)