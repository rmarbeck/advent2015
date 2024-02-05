import scala.io.Source
import com.typesafe.scalalogging.Logger

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching Day21")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def runOn(inputLines: Seq[String]): (String, String) =

    val boss = inputLines.mkString(", ") match
      case s"Hit Points: $hits, Damage: $damage, Armor: $armor" => new Player(hits.toInt, damage.toInt, armor.toInt)
      case _ => throw Exception("Not supported")

    val playerHits = boss.hits.number match
      case 12 => 8
      case _ => 100

    val itemsCombos =
      for
        weapon <- Shop.weapons
        armorItem <- Shop.armorItems
        ring1 <- Shop.rings
        ring2 <- Shop.rings.filterNot(_ == ring1)
      yield
        Items(weapon, armorItem, ring1, ring2)

    val resultPart1 = itemsCombos.sortBy:
      case items: Items => items.cost
    .map(items => (items, Player.from(playerHits, items))).find(_._2.winsVersus(boss)).map(_._1.cost).getOrElse(0)

    val resultPart2 = itemsCombos.sortBy:
      case items: Items => items.cost
    .reverse.map(items => (items, Player.from(playerHits, items))).find(!_._2.winsVersus(boss)).map(_._1.cost).getOrElse(0)

    val result1 = s"$resultPart1"
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

case class Items(weapon: Weapon, armorItem: ArmorItem, ring1: Ring, ring2: Ring):
  lazy val cost = weapon.cost + armorItem.cost + ring1.cost + ring2.cost
  lazy val damage = weapon.damage.number + ring1.damage.number + ring2.damage.number
  lazy val armor = armorItem.armor.number + ring1.armor.number + ring2.armor.number

object Shop:
  val weapons = List(
    Weapon("Dagger", Damage(4), 8),
    Weapon("Shortsword", Damage(5), 10),
    Weapon("Warhammer", Damage(6), 25),
    Weapon("Longsword", Damage(7), 40),
    Weapon("Greataxe", Damage(8), 74)
                )
  val armorItems = List(
    ArmorItem("None 1", Armor(0), 0),
    ArmorItem("None 2", Armor(0), 0),
    ArmorItem("Leather", Armor(1), 13),
    ArmorItem("Chainmail", Armor(2), 31),
    ArmorItem("Splintmail", Armor(3), 53),
    ArmorItem("Bandedmail", Armor(4), 75),
    ArmorItem("Platemail", Armor(5), 102)
  )

  val rings = List(
    Ring("None", Damage(0), Armor(0), 0),
    Ring("Damage +1", Damage(1), Armor(0), 25),
    Ring("Damage +2", Damage(2), Armor(0), 50),
    Ring("Damage +3", Damage(3), Armor(0), 100),
    Ring("Defense +1", Damage(0), Armor(1), 20),
    Ring("Defense +2", Damage(0), Armor(2), 40),
    Ring("Defense +3", Damage(0), Armor(3), 80)
  )

case class Weapon(name: String, damage: Damage, cost: Int)
case class ArmorItem(name: String, armor: Armor, cost: Int)
case class Ring(name: String, damage: Damage, armor: Armor, cost: Int)

case class Hits(number: Int):
  def nbRoundBeforeLosing(lossPerRound: Int): Int =
    val minimum = number / lossPerRound
    number % lossPerRound match
      case 0 => minimum
      case _ => minimum + 1
case class Damage(number: Int):
  def -(armor: Armor): Int = this.number - armor.number
case class Armor(number: Int)

case class Player(hits: Hits, damage: Damage, armor: Armor):
  def this(hits: Int, damage: Int, armor: Int) = this(Hits(hits), Damage(damage), Armor(armor))
  def winsVersus(other: Player): Boolean =
    val perRoundAttack = math.max(this.damage - other.armor, 1)
    val perRoundDefense = math.max(other.damage - this.armor, 1)
    val nbAttack = other.hits.nbRoundBeforeLosing(perRoundAttack)
    val nbDefense = this.hits.nbRoundBeforeLosing(perRoundDefense)
    loggerAOCPart1.trace(s"Nb attack = $nbAttack, nbDefense = $nbDefense")
    nbAttack <= nbDefense

object Player:
  def from(hits: Int, items: Items) =
    Player(Hits(hits), Damage(items.damage), Armor(items.armor))
