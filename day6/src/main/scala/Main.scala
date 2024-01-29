import scala.io.Source
import com.typesafe.scalalogging.Logger

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day6")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =

    val panel: PanelPart1 = PanelPart1()
    val panel2: PanelPart2 = PanelPart2()

    val panels = List(panel, panel2)

    /*inputLines.foreach:
      case s"turn on $start1,$start2 through $end1,$end2" => panels.foreach(_.turnOn((start1.toInt, start2.toInt), (end1.toInt, end2.toInt)))
      case s"turn off $start1,$start2 through $end1,$end2" => panels.foreach(_.turnOff((start1.toInt, start2.toInt), (end1.toInt, end2.toInt)))
      case s"toggle $start1,$start2 through $end1,$end2" => panels.foreach(_.toggle((start1.toInt, start2.toInt), (end1.toInt, end2.toInt)))*/

    inputLines.map:
      case s"turn $switch $other" =>
        val function = switch match
          case "on" => LightPanel.turnOn
          case "off" => LightPanel.turnOff
        (function, other)
      case s"toggle $other" => (LightPanel.toggle, other)
    .foreach:
      case (function, s"$start1,$start2 through $end1,$end2") =>
        val start :: end :: Nil = List(start1, start2, end1, end2).map(_.toInt).grouped(2).map(value => (value(0), value(1))).toList: @unchecked
        panels.foreach(function(_, start, end))
    
    val result1 = s"${panel.howManyOn}"
    val result2 = s"${panel2.brightness}"

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

trait LightPanel:
  def turnOn(start: (Int, Int), end: (Int, Int)): Unit
  def turnOff(start: (Int, Int), end: (Int, Int)): Unit
  def toggle(start: (Int, Int), end: (Int, Int)): Unit

  def change(start: (Int, Int), end: (Int, Int), toDo: (Int, Int) => Unit): Unit =
    for row <- start(0) to end(0)
        col <- start(1) to end(1)
    do
      toDo.apply(row, col)

object LightPanel:
  def turnOn(panel: LightPanel, start: (Int, Int), end: (Int, Int)): Unit = panel.turnOn(start, end)
  def turnOff(panel: LightPanel, start: (Int, Int), end: (Int, Int)): Unit = panel.turnOff(start, end)
  def toggle(panel: LightPanel, start: (Int, Int), end: (Int, Int)): Unit = panel.toggle(start, end)

class PanelPart1 extends LightPanel:
  val lights: Array[Array[Boolean]] = Array.fill(1000, 1000)(false)
  def howManyOn: Int = lights.flatten.count(_ == true)
  def turnOn(start: (Int, Int), end: (Int, Int)): Unit =
    change(start, end, lights(_)(_) = true)
  def turnOff(start: (Int, Int), end: (Int, Int)): Unit =
    change(start, end, lights(_)(_) = false)
  def toggle(start: (Int, Int), end: (Int, Int)): Unit =
    change(start, end, (row, col) => lights(row)(col) = ! lights(row)(col))

class PanelPart2 extends LightPanel:
  val lights: Array[Array[Int]] = Array.fill(1000, 1000)(0)
  def brightness: Int = lights.flatten.sum
  def turnOn(start: (Int, Int), end: (Int, Int)): Unit =
    change(start, end, lights(_)(_) += 1)
  def turnOff(start: (Int, Int), end: (Int, Int)): Unit =
    change(start, end, (row, col) => lights(row)(col) = math.max(0, lights(row)(col) - 1))
  def toggle(start: (Int, Int), end: (Int, Int)): Unit =
    change(start, end, lights(_)(_) += 2)