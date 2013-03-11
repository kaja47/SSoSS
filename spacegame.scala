import scala.reflect.ClassTag
import language.postfixOps
import annotation.tailrec

type Blocks = Map[(Int, Int), Block]

def distance(a: (Int, Int), b: (Int, Int)): Double = {
  val (ax, ay) = a
  val (bx, by) = b
  math.sqrt(math.pow(ax - bx, 2) + math.pow(ay - by, 2))
}

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
  def centerOfMass = {
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

def neighboursOf(pos: (Int, Int)) = {
  val (x,y) = pos
  Seq((x+1,y), (x-1,y), (x,y+1), (x,y-1))
}


trait PhysicalProperties {
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
}


case class Material(name: String, mass: Int, durability: Int) extends PhysicalProperties
object Material {
  val skeleton  = Material("skeleton",  100,  100)
  val titanium  = Material("titanium",  250,  800)
  val tritanium = Material("tritanium", 300, 1200)
}



trait Installation extends PhysicalProperties {
  def mass: Int
  def durability: Int

  def derive(block: Block): Installation = this
  def derive(pos: (Int, Int), blocks: Blocks): Installation = this

  def in(material: Material) = Block(material = material, installation = this)
}

trait ProduceEnergy extends Installation { def energyOutput: Int }
trait ConsumeEnergy extends Installation { def energyInput: Int }


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
  dissipation: Double,
  overchargeEfficiency: Double,
  criticalTemperature: Int
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
  val laser = Beam(mass = 100, durability = 100, damagePerSec = 600, energyInput = 500, range = 50, dissipation = 0.8, overchargeEfficiency = 0.5, criticalTemperature = 1000)
}




val makeBlock = Map[Char, Block](
  '#' -> Block(material = Material.skeleton, installation = PilotCapsule())
, 'P' -> Block(material = Material.titanium, installation = NoInstallation)
, 'E' -> Block(material = Material.skeleton, installation = EnergyCore(100))
, '=' -> Block(material = Material.skeleton, installation = Engine(100, 500000))
, 'L' -> Block(material = Material.skeleton, installation = Beam.laser)
, 'S' -> Block(material = Material.skeleton, installation = Storage(500))
, ' ' -> null
)

def makeShip(str: String) =
  SpaceShip((for {
    (line, y)  <- str.lines.zipWithIndex
    (block, x) <- (line map makeBlock zipWithIndex)
    if block != null
  } yield (x,y) -> block).toMap)


val _ship = makeShip("""
    P
    P     P
PPPPPPPPPPPPPP
    P     P
    P
""")

val enemyShip = makeShip("""
PPPPPPP
    PPPP
     PPPPP
    PPPP
PPPPPPP
""")



import com.vividsolutions.jts.geom
import geom.util.AffineTransformation
def coord(x: Double, y: Double) = new geom.Coordinate(x,y)
val gf = new geom.GeometryFactory
//val l = gf.createLineString(Array(coord(0,0), coord(100,100)))
//l intersection r

val toShape = new com.vividsolutions.jts.awt.ShapeWriter().toShape _
def fromAwtPoint(point: java.awt.Point): geom.Point = gf.createPoint(coord(point.x, point.y))

val blockSize = 25.0

case class ShipInBattle(ship: SpaceShip, pos: geom.Point, rot: Double) {
  private def baseGeom: Map[(Int, Int), geom.Geometry] =
    ship.blocks map { case (p @ (x, y), bl) =>
      val (comx, comy) = ship.centerOfMass

      // absolute offsets from `pos` (centerOfMass)
      val xoff = (x - comx) * blockSize
      val yoff = (y - comy) * blockSize

      val xpos = pos.getX + xoff
      val ypos = pos.getY + yoff
      
      p -> gf.createPolygon(Array(
        coord(xpos,             ypos),
        coord(xpos + blockSize, ypos),
        coord(xpos + blockSize, ypos + blockSize),
        coord(xpos,             ypos + blockSize),
        coord(xpos,             ypos)
      ))
    }
  lazy val rotGeom: Map[(Int, Int), geom.Geometry] = {
    val at = AffineTransformation.rotationInstance(rot, pos.getX, pos.getY)
    baseGeom mapValues at.transform
  }
  lazy val geomColl = gf.createGeometryCollection(rotGeom.values.toArray)
    
}
case class BattleModel(ships: Seq[ShipInBattle], targeting: Map[(Int, (Int, Int)), (Int, (Int, Int))] = Map( (0 -> (2, 13)) -> (1 -> (2, 10)) ) )

def placeShip(ship: SpaceShip, x: Int, y: Int, dir: Double) = ShipInBattle(ship, gf.createPoint(coord(x, y)), dir)





import swing._
import swing.event._
import java.awt.{ Graphics2D, Color, Dimension, Font, FontMetrics, RenderingHints, Point, BasicStroke /*, geom*/ }

// heartbeat functions

def tick(periodMillis: Int)(f: => Unit): Long = {
  val startTime = System.nanoTime
  f
  val endTime = System.nanoTime
  val sleepNanos = periodMillis * 1000000 - (endTime - startTime)
  if (sleepNanos >= 0) {
    Thread.sleep(sleepNanos / 1000000, sleepNanos % 1000000 toInt)
  } else {
    println("tick error: no sleep at all ("+sleepNanos+" ns)")
  }
  endTime - startTime
}

def makeHeartBeatThread(fps: Int)(f: => Any) =
  new Thread(new Runnable {
    val periodMillis = 1000 / fps
    def run: Unit = {
      var total    = 0L
      var measures = 0L
      while(true) { 
        total += tick(periodMillis)(f)
        measures += 1
        if (measures == 5 * fps) {
          printf("%f ms\n", 1.0 * total / measures / 1000000)
          total = 0
          measures = 0
        }
      }
    }
  })

object App extends SwingApplication {
  def startup(args: Array[String]) {
    val t = top
    if (t.size == new Dimension(0, 0)) t.pack()
    t.visible = true
    t.repaint
  }

  def top: MainFrame = new MainFrame {
    title = "space game"
    preferredSize = new Dimension(640, 640)

    centerOnScreen()
    //maximize()

/*
    val shipPanel = new Panel { panel =>

      makeHeartBeatThread(fps = 50) {
        panel.repaint()
      }.start()

      var zoom = 1.0
      var mousePoint = new Point(0, 0)
      var ship = _ship
      def gridDist = 30 * zoom toInt ;

      def posOf(p: Point) = (p.x / gridDist, p.y / gridDist)

      // active square's coordinate
      def pos = posOf(mousePoint)

      listenTo(keys, mouse.clicks, mouse.moves, mouse.wheel)
      reactions += {
        case MouseClicked(source, point, mods, clicks, trigPopup) =>
          val p = posOf(point)
          val bl = Block(material = Material.skeleton, installation = NoInstallation)
          if (ship.blocks contains p) {
            ship = ship copy (blocks = ship.blocks - p)
          } else {
            ship = ship copy (blocks = ship.blocks + ((p, bl)))
          }
        case MouseWheelMoved(source, point, mods, rot) => 
          zoom *= (1 + 0.1 * rot)
          zoom = zoom min 4 max 0.5
        case MouseMoved(source, point, mods) =>
          mousePoint = point
        case _ =>
      }

      override def paint(g: Graphics2D) {
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

        // clear window
        g.setColor(Color.darkGray)
        g.fillRect(0, 0, size.getWidth.toInt, size.getHeight.toInt)

        val w = panel.size.width
        val h = panel.size.height

        // draw border
        g.setColor(Color.GRAY)
        g.drawRect(0, 0, w-1, h-1)

        // draw grid
        for (x <- 0 to w by gridDist) g.drawLine(x, 0, x, h)
        for (y <- 0 to h by gridDist) g.drawLine(0, y, w, y)

        // draw blocks
        g.setColor(Color.red)
        for ((pos, block) <- ship.blocks) {
          val (posx, posy) = pos
          g.drawRect(posx*gridDist, posy*gridDist, gridDist, gridDist)
          g.drawRect(posx*gridDist+1, posy*gridDist+1, gridDist-2, gridDist-2)
        }

        // draw active square
        {
          g.setColor(Color.white)
          val (posx, posy) = pos
          g.drawRect(posx*gridDist, posy*gridDist, gridDist, gridDist)
        }

        // draw zoom
        drawTooltip(g, "zoom: " + zoom.toString.take(5), 10, 10)
        drawTooltip(g, "ship: \n" + ship.info, 10, 50)


        if (ship.blocks contains pos) {
          val bl = ship.blocks(pos)
          drawTooltip(g, s"x: ${pos._1} y: ${pos._2} \n$bl", mousePoint.x-3, mousePoint.y-3, true)
        }
      }
    }
*/

    def rot(sib: ShipInBattle, angle: Double) = sib copy (rot = sib.rot + angle)
    def move(sib: ShipInBattle, x: Double, y: Double) = sib copy (pos = gf.createPoint(coord(sib.pos.getX, sib.pos.getY)))

    val battlePanel = new Panel { panel =>
      listenTo(keys, mouse.clicks, mouse.moves, mouse.wheel)
      reactions += {
        case KeyPressed(src, Key.A, mods, loc) => battleModel = battleModel.copy(ships = battleModel.ships map (s => rot(s,  0.04)))
        case KeyPressed(src, Key.D, mods, loc) => battleModel = battleModel.copy(ships = battleModel.ships map (s => rot(s, -0.04)))
        case KeyPressed(src, Key.W, mods, loc) => battleModel = battleModel.copy(ships = battleModel.ships map (s => move(s, 0, 5)))
        case MouseMoved(src, point, mods) =>
          mousePoint = point
          val p = scaleViewToModel(fromAwtPoint(point))
          val hoveredBlockPos = for {
            sh <- battleModel.ships
            (blpos, bl) <- sh.rotGeom
            if bl.contains(p)
          } yield (sh, blpos)
          hoveredBlockPos.headOption match {
            case Some((sh, blpos)) => hover = sh.ship.blocks(blpos)
            case None              => hover = null
          }
        case MouseDragged(src, point, mods) =>
          scale.translate(point.x - mousePoint.x, point.y - mousePoint.y)
          mousePoint = point
        case MouseWheelMoved(source, point, mods, rot) => 
          val delta = (1 + 0.1 * rot)
          zoom *= delta
          zoom = zoom min 10 max 0.05
          scale.compose(AffineTransformation.scaleInstance(delta, delta, point.x, point.y))
          println(scale.getMatrixEntries.toVector)

        case MouseClicked(source, point, mods, clicks, trigPopup) =>
          ;
      }

      var zoom = 0.25
      var translationX, translationY = 0
      var mousePoint = new Point(0, 0)
      var hover: AnyRef = _
      var battleModel: BattleModel = BattleModel(Seq(placeShip(_ship, 100, 100, 0), placeShip(enemyShip, 1000, 200, math.Pi)))

      var scale = new AffineTransformation()
      def scaleModelToView(g: geom.Point): geom.Point       = scale.transform(g).asInstanceOf[geom.Point]
      def scaleModelToView(g: geom.Geometry): geom.Geometry = scale.transform(g)
      def scaleViewToModel(g: geom.Point): geom.Point       = scale.getInverse.transform(g).asInstanceOf[geom.Point]
      def scaleViewToModel(g: geom.Geometry): geom.Geometry = scale.getInverse.transform(g)


      makeHeartBeatThread(fps = 25) {
        panel.repaint()
      }.start()

      override def paint(g: Graphics2D) {
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

        val panelWidth  = size.getWidth.toInt
        val panelHeight = size.getHeight.toInt

        // draw stars
        g.setColor(Color.BLACK)
        g.fillRect(0, 0, panelWidth, panelHeight)

        val r = new util.Random(152)
        for (i <- 1 to 3000) {

          val size = {
            val i = r.nextInt(100)
                 if (i <= 84) 1
            else if (i <= 94) 2
            else if (i <= 98) 3
            else              4
          }

          def color = r.nextInt(75)+180
          g.setColor(new Color(color, color, color, color + (size-1) * 20 min 255))

          val x = r.nextInt(panelWidth)
          val y = r.nextInt(panelHeight)
          g.fillOval(x, y, size, size)
          if (i % 10 == 0 && size == 3) {
            g.drawLine(x-3+1, y+1, x+3+1, y+1)
            g.drawLine(x+1, y-3+1, x+1, y+3+1)
          }
        }

        // draw ships
        for (sib @ ShipInBattle(ship, pos, rot) <- battleModel.ships) {

          val hull = sib.geomColl.convexHull
          val a = hull.buffer(15)
          val b = hull.buffer(14.5)
          val outline = new Color(0x6495ed)
          val backgr  = new Color(173, 216, 230, 50)

          g.setColor(backgr)
          g.fill(toShape(scaleModelToView(a)))

          g.setColor(new Color(255,255,255,180))
          g.draw(toShape(scaleModelToView(b)))

          g.setColor(outline)
          g.draw(toShape(scaleModelToView(a)))

          for (((x,y), geom) <- sib.rotGeom) {
            g.setColor(Color.WHITE)
            g.fill(toShape(scaleModelToView(geom)))
            g.setColor(Color.GRAY)
            g.draw(toShape(scaleModelToView(geom)))
          }

          g.setColor(Color.RED)
          val _pos = scaleModelToView(pos)
          val x = _pos.getX.toInt
          val y = _pos.getY.toInt
          g.drawLine(x-10, y,    x+10, y)
          g.drawLine(x,    y-10, x,    y+10)
          g.drawOval(x-10, y-10, 20, 20)
          drawTooltip(g, s"com "+pos, x, y, true)
        }

        // lazors

        g.setColor(new Color(255, 0, 0, 150))
        g.setStroke(new BasicStroke(6));
        g.drawLine(100, 100, 1000, 200)

        g.setColor(Color.RED)
        g.setStroke(new BasicStroke(4));
        g.drawLine(100, 100, 1000, 200)

        g.setStroke(new BasicStroke(1));
        g.setColor(Color.WHITE)
        g.drawLine(100, 100, 1000, 200)

        if (hover != null)
          drawTooltip(g, ""+hover, mousePoint.x, mousePoint.y, true)
      }
    }


//    contents = new BorderPanel {
//      layout += (shipPanel -> BorderPanel.Position.Center)
//      layout += (new Panel {} -> BorderPanel.Position.East)
//    }
    contents = battlePanel
    battlePanel.requestFocus()
  }
}

def drawTooltip(g: Graphics2D, str: String, x: Int, y: Int, upleft: Boolean = false) = {
  val lines = str.lines
  val metrics = g.getFontMetrics
  val lineHeight = metrics.getHeight

  for ((line, idx) <- lines.zipWithIndex) {
    val lineWidth = metrics.stringWidth(line)

    val boxw = lineWidth+10
    val boxh = lineHeight+5

    val xx = math.max(if (upleft) x - boxw else x, 0)
    val yy = math.max(if (upleft) y - boxh else y, 0) + (lineHeight + 5) * idx

    g.setColor(new Color(0,0,0,200))
    g.fillRect(xx, yy, boxw, boxh)

  //  g.setColor(Color.GRAY)
  //  g.drawRect(xx, yy, boxw, boxh)

    g.setColor(new Color(255,255,255, 200))
    g.drawString(line, xx+5, yy+lineHeight)
  }
}

App.main(Array())
