import scala.io.Source
import com.typesafe.scalalogging.Logger

import scala.annotation.tailrec

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day23")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =

    val instructions: Array[Instruction] = inputLines.map:
      case s"jmp $offset" => Jump(offset.toInt)
      case s"$instr $aOrB, $offset" =>
        instr match
          case "jie" => JumpIfEven(AorB.from(aOrB.head), offset.toInt)
          case "jio" => JumpIfOne(AorB.from(aOrB.head), offset.toInt)
          case _ => throw Exception(s"Instruction not recognized ${instr}")
      case s"$instr $aOrB" =>
        instr match
          case "hlf" => Half(AorB.from(aOrB.head))
          case "tpl" => Triple(AorB.from(aOrB.head))
          case "inc" => Increments(AorB.from(aOrB.head))
          case _ => throw Exception(s"Instruction not recognized ${instr}")
    .toArray

    val resultPart1 = runInstructions(instructionsAvailable = instructions)
    val resultPart2 = runInstructions((1, 0, 0), instructionsAvailable = instructions)

    val result1 = s"${resultPart1._2}"
    val result2 = s"${resultPart2._2}"

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

@tailrec
def runInstructions(status: (Long, Long, Int) = (0l,0l,0), instructionsAvailable: Array[Instruction]): (Long, Long) =
  loggerAOCPart1.trace(s"$status")
  val (currentAValue, currentBValue, currentIndex) = status
  val maxInstructionIndex = instructionsAvailable.size - 1
  currentIndex match
    case -1 => (currentAValue, currentBValue)
    case value if value > maxInstructionIndex => (currentAValue, currentBValue)
    case _ =>
      runInstructions(instructionsAvailable(currentIndex).run.tupled(status), instructionsAvailable)

sealed trait AorB

object AorB:
  def from(aOrBAsChar: Char): AorB =
    aOrBAsChar match
      case 'a' => A()
      case 'b' => B()
      case _ => throw Exception("Not managed")
class A extends AorB:
  override def toString: String = "A"
class B extends AorB:
  override def toString: String = "B"


sealed trait Instruction(val aOrB: AorB):
  def action(fromAValue: Long, fromIndex: Int): (Long, Int)
  def run(aValue: Long, bValue: Long, currentIndex: Int): (Long, Long, Int) =
    aOrB match
      case c: A =>
        val (regAOut, outIndex) = action(aValue, currentIndex)
        (regAOut, bValue, outIndex)
      case c: B =>
        val (regBOut, outIndex) = action(bValue, currentIndex)
        (aValue, regBOut, outIndex)

case class Half(aOrBValue: AorB) extends Instruction(aOrBValue):
  override def action(fromValue: Long, fromIndex: Int) = (fromValue / 2, fromIndex + 1)
case class Triple(aOrBValue: AorB) extends Instruction(aOrBValue):
  override def action(fromValue: Long, fromIndex: Int) = (fromValue * 3, fromIndex + 1)
case class Increments(aOrBValue: AorB) extends Instruction(aOrBValue):
  override def action(fromValue: Long, fromIndex: Int) = (fromValue + 1, fromIndex + 1)

case class Jump(offset: Int) extends Instruction(A()):
  override def action(fromValue: Long, fromIndex: Int) = (fromValue, fromIndex + offset)
case class JumpIfEven(aOrBValue: AorB, offset: Int) extends Instruction(aOrBValue):
  override def action(fromValue: Long, fromIndex: Int) =
    fromValue % 2 == 0 match
      case true => (fromValue, fromIndex + offset)
      case false => (fromValue, fromIndex + 1)
case class JumpIfOne(aOrBValue: AorB, offset: Int) extends Instruction(aOrBValue):
  override def action(fromValue: Long, fromIndex: Int) =
    fromValue == 1 match
      case true => (fromValue, fromIndex + offset)
      case false => (fromValue, fromIndex + 1)
