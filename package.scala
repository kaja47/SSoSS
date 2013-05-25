package ssoss

import language.postfixOps
import Game._
import com.vividsolutions.jts.geom
import com.vividsolutions.jts.geom.util.AffineTransformation
import com.vividsolutions.jts.algorithm.Angle

object `package` {

  type Blocks = Map[(Int, Int), Block]

  val blockSize = 25.0

  def distance(a: (Int, Int), b: (Int, Int)): Double = {
    val (ax, ay) = a
    val (bx, by) = b
    math.sqrt(math.pow(ax - bx, 2) + math.pow(ay - by, 2))
  }

  def neighboursOf(pos: (Int, Int)) = {
    val (x,y) = pos
    Seq((x+1,y), (x-1,y), (x,y+1), (x,y-1))
  }



  private val geometryFactory = new geom.GeometryFactory
  private val shapeWriter = new com.vividsolutions.jts.awt.ShapeWriter()

  def mkCoord(x: Double, y: Double) = new geom.Coordinate(x,y)
  def mkPoint(x: Double, y: Double) = geometryFactory.createPoint(mkCoord(x, y))
  def mkLine(a: geom.Point, b: geom.Point) = geometryFactory.createLineString(Array(a.getCoordinate, b.getCoordinate))
  def mkRect(xpos: Double, ypos: Double, blockSize: Double) = 
    geometryFactory.createPolygon(Array(
      mkCoord(xpos,             ypos),
      mkCoord(xpos + blockSize, ypos),
      mkCoord(xpos + blockSize, ypos + blockSize),
      mkCoord(xpos,             ypos + blockSize),
      mkCoord(xpos,             ypos)
    ))
  def mkGeometryCollection(gs: Array[geom.Geometry]) = geometryFactory.createGeometryCollection(gs)

  /** converts jts Geometry to awt Shape */
  def toAwtShape(g: geom.Geometry) = shapeWriter.toShape(g)
  def toAwtPoint(p: geom.Point) = new java.awt.Point(p.getX.toInt, p.getY.toInt)

  def awtPointToJts(point: java.awt.Point): geom.Point =
    geometryFactory.createPoint(mkCoord(point.x, point.y))

  def awtShapeToJts(sh: java.awt.Shape): geom.Geometry =
    new com.vividsolutions.jts.awt.ShapeReader(geometryFactory).read(sh.getPathIterator(new java.awt.geom.AffineTransform()))


  def move(from: geom.Point, targ: geom.Point, dist: Double) =
    if ((from distance targ) < dist) targ else {
      val dir = Angle.angle(from.getCoordinate, targ.getCoordinate)
      val scale = AffineTransformation.translationInstance(dist, 0)
      scale.compose(AffineTransformation.rotationInstance(dir, from.getX, from.getY))
      scale.transform(from).asInstanceOf[geom.Point]
    }

  def moveInDir(from: geom.Point, dist: Double, dir: Double) = {
    val scale = new AffineTransformation()
    scale.compose(AffineTransformation.translationInstance(dist, 0))
    scale.compose(AffineTransformation.rotationInstance(dir, from.getX, from.getY))
    scale.transform(from).asInstanceOf[geom.Point]
  }

  def rot(srcRot: Double, destRot: Double, rotBy: Double) = {
    val diff = destRot - srcRot
  
    if      (diff < rotBy)   destRot
    else if (diff < math.Pi) srcRot + rotBy
    else                     srcRot - rotBy
  }

}
