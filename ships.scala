package ssoss

import scala.reflect.ClassTag
import language.postfixOps
import Game._
import java.awt.Color


case class SpaceShip(
    blocks: Blocks,
    storageManifest: StorageManifest = StorageManifest(0, 0, 0)
) {
  def block(x: Int, y: Int) = blocks((x, y))

  // first apply modifications by local state of block and the
  def derivedInstallations: Map[(Int, Int), Installation] = {
    val derivedBlocks = blocks mapValues { block => block copy (installation = block.installation.derive(block)) }
    derivedBlocks map { case (pos, block) => pos -> block.installation.derive(pos, derivedBlocks) }
  }

  def _baseInstallations = blocks mapValues (_.installation)

  def installations[T <: Installation : ClassTag]: Iterable[T] = derivedInstallations.values collect { case b: T => b }
  def installations[T <: Installation : ClassTag, R](f: T => R): Iterable[R] = installations[T] map f

  def size = blocks.size
  def mass = blocks.values map (_.mass) sum
  def xLength = if (size == 0) 0 else (blocks.keys map (_._1) max) - (blocks.keys map (_._1) min) + 1
  def yLength = if (size == 0) 0 else (blocks.keys map (_._2) max) - (blocks.keys map (_._2) min) + 1
  def centerOfMass: (Int, Int) = {
    var sx, sy = 0
    for (((x, y), b) <- blocks) {
      sx += x * b.mass
      sy += y * b.mass
    }
    val m = mass
    if (m == 0) (0, 0) else (sx / mass, sy / mass)
  }
  def hasEngine   = installations[Engine] nonEmpty
  def travelSpeed = if (hasEngine) installations[Engine, Int](_.power).sum / mass else 0
  def turnSpeed   = travelSpeed // for now
  def storageCapacity = installations[Storage, Int](_.capacity) sum
  def energyOutput = installations[ProduceEnergy, Int](_.energyOutput) sum
  def energyInput  = installations[ConsumeEnergy, Int](_.energyInput) sum

  /** check is ship is one compact object */
  def isCompact = {
    val visited = collection.mutable.Set[(Int, Int)]()
    def traverse(pos: (Int, Int)): Unit = {
      visited += pos
      for {
        n <- neighboursOf(pos)
        if !visited(n) && (blocks contains n)
      } traverse(n)
    }
    traverse(blocks.keys.head)
    visited.size == blocks.size
  }

  def info = s"""
size: $size
mass: $mass
xLength: $xLength
yLength: $yLength
centerOfMass: $centerOfMass
hasEngine: $hasEngine
travelSpeed: $travelSpeed
turnSpeed: $turnSpeed
storageCapacity: $storageCapacity
energyOutput: $energyOutput
energyInput: $energyInput
isCompact: $isCompact
""".trim

}


case class StorageManifest(minerals: Int, fuel: Int, parts: Int)

sealed trait PhysicalProperties {
  def mass: Int
  def durability: Int
}


case class Block(
  material: Material,
  installation: Installation,

  damage: Int        = 0,
  overcharge: Double = 1.0,
  temperature: Int   = 0
) extends PhysicalProperties {
  def mass       = material.mass       + installation.mass
  def durability = material.durability + installation.durability

  def isActive = installation match { case _: ConsumeEnergy => true; case _ => false }

  override def toString = s"block: material = $material, installation = $installation, dmg = $damage, overcharge = $overcharge, temperature = $temperature"
}


case class Material(name: String, mass: Int, durability: Int) extends PhysicalProperties
object Material {
  val skeleton  = Material("skeleton",  100,  100)
  val titanium  = Material("titanium",  250,  800)
  val tritanium = Material("tritanium", 300, 1200)
}



sealed trait Installation extends PhysicalProperties {
  def mass: Int
  def durability: Int

  def derive(block: Block): Installation = this
  def derive(pos: (Int, Int), blocks: Blocks): Installation = this

  def in(material: Material) = Block(material = material, installation = this)
}

sealed trait ProduceEnergy extends Installation { def energyOutput: Int }
sealed trait ConsumeEnergy extends Installation { def energyInput: Int }


case object NoInstallation extends Installation { val mass = 0; val durability = 0 }

case class PilotCapsule() extends Installation {
  val mass = 100
  val durability = 1000
}
case class EnergyCore(energyOutput: Int) extends Installation with ProduceEnergy {
  val mass = 100
  val durability = 100
  override def derive(block: Block): EnergyCore = copy(
    energyOutput = 1.0 * energyOutput * (1.0 - block.damage / block.durability) toInt
  )
  override def derive(pos: (Int, Int), blocks: Blocks): EnergyCore = {
    val bonus = 1.0 + (for {
      n <- neighboursOf(pos)
      Block(_, e: EnergyCore, _, _, _) <- blocks.get(n)
    } yield 0.1).sum
    copy(energyOutput = energyOutput * bonus toInt)
  }
}
case class Engine(energyInput: Int, power: Int) extends Installation with ConsumeEnergy {
  val mass = 100
  val durability = 500;
  override def derive(block: Block): Engine = copy(
    energyInput = 1.0 * energyInput * block.overcharge toInt,
    power       = 1.0 * power * (1.0 - block.damage / block.durability) * block.overcharge toInt
  )
}
case class Storage     (capacity: Int)                extends Installation { val mass = 100; val durability = 100 }
case class Beam(
  mass: Int,
  durability: Int,
  damagePerSec: Int,
  energyInput: Int,
  range: Double,
  dissipation: Double, // damage is inversly linearlly proportional to range, dissiapation is ratio of damage caused by beam on the very end of it's range
  overchargeEfficiency: Double,
  criticalTemperature: Int,
  color: Color
  // beamCount
) extends Installation with ConsumeEnergy {
  override def derive(block: Block): Beam = copy(
    damagePerSec = 1.0 * damagePerSec *
      (1.0 - block.damage / block.durability) *
      math.min(block.temperature, criticalTemperature) / block.temperature *
      (if (block.overcharge > 1) 1 + (block.overcharge - 1) * overchargeEfficiency else block.overcharge ) toInt,
    energyInput  = 1.0 * energyInput * block.overcharge toInt
  )
}

object Beam {
  val laser  = Beam(mass = 100, durability = 100, damagePerSec = 600, energyInput = 500, range = 500, dissipation = 0.8,  overchargeEfficiency = 0.5, criticalTemperature = 1000, color = Color.RED)
  val phaser = Beam(mass = 200, durability = 100, damagePerSec = 900, energyInput = 500, range = 400, dissipation = 0.6,  overchargeEfficiency = 0.5, criticalTemperature = 1000, color = Color.ORANGE)
  val gauss  = Beam(mass = 400, durability = 100, damagePerSec = 600, energyInput = 500, range = 800, dissipation = 0.95, overchargeEfficiency = 0.5, criticalTemperature = 1000, color = Color.GRAY)
}
