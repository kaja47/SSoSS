package ssoss

import scala.reflect.ClassTag
import language.postfixOps
import Game._
import java.awt.Color


final case class SpaceShip(
    blocks: Blocks,
    storageManifest: StorageManifest = StorageManifest(0, 0, 0)
) {
  //def block(x: Int, y: Int) = blocks((x, y))

  // first apply modifications by local state of block and then by state of whole (derived) ship
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
  def hasEngine       = installations[Engine] nonEmpty
  /** speed in units per second */
  def travelSpeed     = if (hasEngine) installations[Engine, Double](_.power).sum / mass else 0
  /** speed in radians per second */
  def turnSpeed       = travelSpeed // for now
  def storageCapacity = installations[Storage, Int](_.capacity) sum
  def energyOutput    = installations[ProduceEnergy, Double](_.energyOutput) sum
  def energyInput     = installations[ConsumeEnergy, Double](_.energyInput) sum

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


final case class StorageManifest(minerals: Int, fuel: Int, parts: Int)

sealed trait PhysicalProperties {
  def mass: Int
  def durability: Int
}


final case class Block(
  material: Material,
  installation: Installation,

  damage: Double      = 0.0,
  overcharge: Double  = 1.0,
  temperature: Double = 0.0
) extends PhysicalProperties {
  def mass       = material.mass       + installation.mass
  def durability = material.durability + installation.durability

  def isActive = installation match { case _: ConsumeEnergy => true; case _ => false }

  override def toString = s"block: material = $material, installation = $installation, dmg = $damage, overcharge = $overcharge, temperature = $temperature"
}


final case class Material(name: String, mass: Int, durability: Int) extends PhysicalProperties
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

sealed trait ProduceEnergy extends Installation { def energyOutput: Double }
sealed trait ConsumeEnergy extends Installation { def energyInput: Double }


case object NoInstallation extends Installation { val mass = 0; val durability = 0 }

final case class PilotCapsule() extends Installation {
  val mass = 100
  val durability = 1000
}
final case class EnergyCore(energyOutput: Double) extends Installation with ProduceEnergy {
  val mass = 100
  val durability = 100
  override def derive(block: Block): EnergyCore = copy(
    energyOutput = 1.0 * energyOutput * (1.0 - block.damage / block.durability)
  )
  override def derive(pos: (Int, Int), blocks: Blocks): EnergyCore = {
    val bonus = 1.0 + (for {
      n <- neighboursOf(pos)
      Block(_, e: EnergyCore, _, _, _) <- blocks.get(n)
    } yield 0.1).sum
    copy(energyOutput = energyOutput * bonus)
  }
}
final case class Engine(energyInput: Double, power: Double) extends Installation with ConsumeEnergy {
  val mass = 100
  val durability = 500;
  override def derive(block: Block): Engine = copy(
    energyInput = 1.0 * energyInput * block.overcharge,
    power       = 1.0 * power * (1.0 - block.damage / block.durability) * block.overcharge
  )
}
final case class Storage     (capacity: Int)                extends Installation { val mass = 100; val durability = 100 }
final case class Beam(
  name: String,
  mass: Int,
  durability: Int,
  damagePerSec: Double,
  // + impactHeat
  energyInput: Double,
  range: Double,
  dissipation: Double, // damage is inversly linearly proportional to range, dissipation is ratio of beam damage lost on the very end of weapon range
  overchargeEfficiency: Double,
  criticalTemperature: Double,
  color: Color
  // beamCount
) extends Installation with ConsumeEnergy {
  def damagePerSecAtDistance(dist: Double) = damagePerSec * (1.0 - (dissipation * (dist / range)))

  override def derive(block: Block): Beam = copy(
    damagePerSec = 1.0 * damagePerSec *
      (1.0 - block.damage / block.durability) *
      (if (block.temperature == 0) 1.0 else math.min(block.temperature, criticalTemperature) / block.temperature) *
      (if (block.overcharge > 1) 1 + (block.overcharge - 1) * overchargeEfficiency else block.overcharge),
    energyInput  = 1.0 * energyInput * block.overcharge
  )
}

object Beam {
  val laser  = Beam("Laser Cannon", mass = 100, durability = 100, damagePerSec = 120, energyInput = 500, range = 1500, dissipation = 0.2,  overchargeEfficiency = 0.5, criticalTemperature = 1000, color = Color.RED)
  val phaser = Beam("Fusion Beam",  mass = 200, durability = 100, damagePerSec = 180, energyInput = 700, range = 1200, dissipation = 0.4,  overchargeEfficiency = 0.5, criticalTemperature = 1000, color = Color.ORANGE)
  val gauss  = Beam("Gauss Cannon", mass = 400, durability = 100, damagePerSec = 120, energyInput = 900, range = 2400, dissipation = 0.05, overchargeEfficiency = 0.5, criticalTemperature = 1000, color = Color.GRAY)

/*
Neutron Blaster
Graviton Beam
Phasor
Plasma Cannon
Mauler Device

Mass Driver
Gauss Cannon

Ion Pulse Cannon
*/


}
