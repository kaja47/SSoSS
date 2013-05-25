package ssoss

import language.postfixOps
import Game._

import com.vividsolutions.jts.geom
import com.vividsolutions.jts.geom.util.AffineTransformation
import com.vividsolutions.jts.algorithm.Angle

case class ShipInBattle(ship: SpaceShip, pos: geom.Point, rot: Double/*, destPos: geom.Point, destRot: geom.Point*/) {
  private def baseGeom: Map[(Int, Int), geom.Geometry] =
    ship.blocks map { case (p @ (x, y), bl) =>
      val (comx, comy) = ship.centerOfMass

      // absolute offsets from `pos` (centerOfMass)
      val xoff = (x - comx) * blockSize
      val yoff = (y - comy) * blockSize

      val xpos = pos.getX + xoff
      val ypos = pos.getY + yoff
      
      p -> mkRect(xpos, ypos, blockSize)
    }

  /** get geometry of this ship rotated to proper angle */
  lazy val blockGeom: Map[(Int, Int), geom.Geometry] = {
    val at = AffineTransformation.rotationInstance(rot, pos.getX, pos.getY)
    baseGeom mapValues at.transform
  }

  lazy val geomColl = mkGeometryCollection(blockGeom.values.toArray)
}

case class BattleModel(
    ships: Seq[ShipInBattle],
    targeting: Map[(Int, (Int, Int)), (Int, (Int, Int))] = Map(),
    movementDirections: Map[Int, (geom.Point, Double)] = Map()
) {
  def withTargeting(source: (Int, (Int, Int)), target: (Int, (Int, Int))): BattleModel =
    if (!canTarget(source, target)) this else copy(targeting = targeting + ((source, target)))

  def withMovementDirections(shipIdx: Int, dest: geom.Point/*, rot: Double*/): BattleModel = {
    val rot = if (movementDirections contains shipIdx) movementDirections(shipIdx)._2 else ships(shipIdx).rot
    copy(movementDirections = movementDirections + ((shipIdx, (dest, rot))))
  }

  def withRotation(shipIdx: Int, rot: Double): BattleModel = {
    val dest = movementDirections(shipIdx)._1
    copy(movementDirections = movementDirections + ((shipIdx, (dest, rot))))
  }

  def blockRotGeom(block: (Int, (Int, Int))) = ships(block._1).blockGeom(block._2)

  def tick(time: Int): BattleModel = {

    val _ships = ships.zipWithIndex map { case (shipInBattle, idx) =>
      if (movementDirections contains idx) {
        val (moveDest, rotDest) = movementDirections(idx)
        shipInBattle copy (
          pos = move(shipInBattle.pos, moveDest, 1),
          rot = rot(shipInBattle.rot, rotDest, 0.02)
        )
      } else shipInBattle
    }

    copy (
      ships     = _ships,
      targeting = targeting filter { case (src, targ) => canTarget(src, targ) }
    )
  }

  private def canTarget(source: (Int, (Int, Int)), target: (Int, (Int, Int))) = {
    val start = blockRotGeom(source).getCentroid
    val end   = blockRotGeom(target).getCentroid
    val line = mkLine(start, end)

    val targetItself = source._1 == target._1

    val bls = ships(source._1).blockGeom - source._2 values
    val collideWithItself = bls exists { bl => line crosses bl }

    val isBeamInRange = ships(source._1).ship.blocks(source._2).installation match {
      case b: Beam if (start distance end) <= b.range => true
      case _ => false
    }

    !targetItself && !collideWithItself && isBeamInRange
  }
}
