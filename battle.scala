package ssoss

import language.postfixOps
import Game._

import com.vividsolutions.jts.geom
import com.vividsolutions.jts.geom.util.AffineTransformation
import com.vividsolutions.jts.algorithm.Angle

case class ShipInBattle(ship: SpaceShip, pos: geom.Point, rot: Double/*, destPos: geom.Point, destRot: geom.Point*/) {
  private def baseGeom: Map[(Int, Int), geom.Geometry] =
    ship.blocks map { case (p @ (x, y), bl) =>
      //val (comx, comy) = ship.centerOfMass

      // absolute offsets from `pos` (centerOfMass)
      val xoff = (x/* - comx*/) * blockSize
      val yoff = (y/* - comy*/) * blockSize

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

  lazy val beamHits: Map[(Int, (Int, Int)), (Int, (Int, Int))] = 
    targeting map { case (src, dest) => (src, collideWith(src, dest)) }

  def collideWith(source: (Int, (Int, Int)), target: (Int, (Int, Int))): (Int, (Int, Int)) = {
    val beam = beamLine(source, target)

    val collisions = for {
      (sib, shipIdx) <- ships.zipWithIndex
      (blpos, blGeom) <- sib.blockGeom.toSeq
      if (shipIdx, blpos) != source // beam cannot damage it's own source block
      if beam crosses blGeom
    } yield (shipIdx, blpos)

    collisions minBy { targ => beamDist(source, targ) }
  }

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

  def shipBlockGeom(block: (Int, (Int, Int))) = ships(block._1).blockGeom(block._2)
  def shipBlock(block: (Int, (Int, Int))) = ships(block._1).ship.blocks(block._2)

  /**
   * @param time - time in seconds (time between frames multiplied by game speed)
   */
  def tick(time: Double): BattleModel = {

    // update of ships in battle
    val _ships = ships.zipWithIndex map { case (sib, idx) => // sib = shipInBattle

      // movement and rotation
      val (_pos, _rot) = if (movementDirections contains idx) {
        val (moveDest, rotDest) = movementDirections(idx)
        val moveDist = sib.ship.travelSpeed * time
        val rotAngle = sib.ship.turnSpeed   * time
        (move(sib.pos, moveDest, moveDist), rot(sib.rot, rotDest, rotAngle))
      } else (sib.pos, sib.rot)

      // beam damage
      val targetedBy: Map[(Int, Int), Double] = beamHits.toSeq collect {
        case (src @ (srcShipIdx, srcBlockPos), targ @ (`idx`, targBlockPos)) =>
          val srcBeam = ships(srcShipIdx).ship.derivedInstallations(srcBlockPos).asInstanceOf[Beam]
          (targBlockPos, srcBeam.damagePerSecAtDistance(beamDist(src, targ)))
      } groupBy (_._1) mapValues { vs => vs map (_._2) sum }

      val _ship = sib.ship.copy(
        blocks = sib.ship.blocks.map {
          case (blpos, bl) if targetedBy contains blpos =>
            //println(targetedBy(blpos) * time)
            blpos -> bl.copy(
              damage = bl.damage + (targetedBy(blpos) * time)
            )
          case p => p
        } filter { case (blpos, bl) => bl.durability > bl.damage }
      )

      // temperature

      sib copy (
        pos  = _pos,
        rot  = _rot,
        ship = _ship
      )
    }

    val updated = copy (
      ships     = _ships
    ) 

    updated copy (
      targeting = updated.targeting filter { case (src, targ) => updated.canTarget(src, targ) }
    )
  }

  private def beamLine(source: (Int, (Int, Int)), target: (Int, (Int, Int))) =
    mkLine(shipBlockGeom(source).getCentroid, shipBlockGeom(target).getCentroid)

  private def beamDist(source: (Int, (Int, Int)), target: (Int, (Int, Int))) =
    shipBlockGeom(source).getCentroid distance shipBlockGeom(target).getCentroid

  private def canTarget(source: (Int, (Int, Int)), target: (Int, (Int, Int))): Boolean = {
    val sourceExists = ships.isDefinedAt(source._1) && ships(source._1).ship.blocks.contains(source._2)
    val targetExists = ships.isDefinedAt(target._1) && ships(target._1).ship.blocks.contains(target._2)

    if (!sourceExists || !targetExists) return false

    val start = shipBlockGeom(source).getCentroid
    val end   = shipBlockGeom(target).getCentroid
    val line = mkLine(start, end)

    val targetItself = source._1 == target._1

    val bls = ships(source._1).blockGeom - source._2 values
    val collideWithItself = bls exists { bl => line crosses bl }

    val isBeamInRange = ships(source._1).ship.derivedInstallations(source._2) match {
      case b: Beam if (start distance end) <= b.range => true
      case _ => false
    }

    !targetItself && !collideWithItself && isBeamInRange
  }
}
