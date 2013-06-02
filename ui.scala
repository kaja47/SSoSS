package ssoss

import language.postfixOps
import Game._

import swing._
import swing.event._
import java.awt.{ Graphics2D, Color, Dimension, Font, FontMetrics, RenderingHints, Shape, Point, Rectangle, BasicStroke /*, geom*/ }
import java.awt.event.InputEvent.{ BUTTON1_DOWN_MASK => LeftMouseButton, BUTTON2_DOWN_MASK => MiddleMouseButton, BUTTON3_DOWN_MASK => RightMouseButton }
import com.vividsolutions.jts.geom
import geom.util.AffineTransformation

object App extends scala.swing.SwingApplication {
  def startup(args: Array[String]) {
    val t = top
    if (t.size == new Dimension(0, 0)) t.pack()
    t.visible = true
    t.repaint
  }

  def top: MainFrame = new MainFrame {
    title = "some sort of space game"

    preferredSize = new Dimension(800, 600)
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

  val battlePanel = new Panel { panel =>
    val mouseButtons = LeftMouseButton | RightMouseButton | MiddleMouseButton

    listenTo(keys, mouse.clicks, mouse.moves, mouse.wheel)
    reactions += {
      case KeyPressed(src, Key.A | Key.Left,  mods, loc) => scale.translate( 10,   0)
      case KeyPressed(src, Key.D | Key.Right, mods, loc) => scale.translate(-10,   0)
      case KeyPressed(src, Key.W | Key.Up,    mods, loc) => scale.translate(  0,  10)
      case KeyPressed(src, Key.S | Key.Down,  mods, loc) => scale.translate(  0, -10)
      case KeyPressed(src, Key.Space, mods, loc) => paused = !paused
      case KeyPressed(src, key, mods, loc) if key == Key.Plus  || key.id == 107 => speed *= 1.2 // integer id matches numpad +/-
      case KeyPressed(src, key, mods, loc) if key == Key.Minus || key.id == 109 => speed /= 1.2

      case MouseMoved(src, point, mods) =>
        mousePoint = point

      case MouseDragged(src, point, mods) =>
        (mods & mouseButtons) match {
          case LeftMouseButton => // selection
            if (selectionStart == null) selectionStart = point
            
          case MiddleMouseButton =>
            if (selectionPath == null) {
              selectionPath = new java.awt.geom.GeneralPath()
              selectionPath.moveTo(point.x, point.y)
            }
            selectionPath.lineTo(point.x, point.y)

          case RightMouseButton =>
            scale.translate(point.x - mousePoint.x, point.y - mousePoint.y)
        }
        mousePoint = point

      case MouseWheelMoved(source, point, Key.Modifier.Control, rot) => 
        scale.rotate(0.1 * rot, point.x, point.y)

      case MouseWheelMoved(source, point, mods, rot) => 
        val delta = (1 + 0.1 * rot)
        zoom *= delta
        scale.compose(AffineTransformation.scaleInstance(delta, delta, point.x, point.y))

      case MouseClicked(source, point, mods, clicks, trigPopup) =>
        val p = scaleViewToModel(awtPointToJts(point))
        hoverOverPoint(p) match {
          case Some(selected @ (sh @ 0, blpos)) if clicks > 1 => // doubleclick select all blocks of player ship
            selectedBlocks ++= (battleModel.ships(sh).ship.blocks.keys map (sh -> _))
          case Some(selected @ (sh @ 0, blpos)) => // selected block from player's ship
            selectedBlocks += selected
          case Some(selected @ (sh, blpos)) => // clicked od enemy ship
            battleModel = selectedBlocks.foldLeft(battleModel) { (bm, s) => bm.withTargeting(s, selected) }
          case None if mods == 0 => // left click on empty space => move
            battleModel = battleModel.withMovementDirections(0, p)
          case None => // right click on empty space => deselect
            selectedBlocks = Set()
        }

      case MouseReleased(src, point, mods, clicks, _) =>
        if (selectionStart != null) {
          selectedBlocks ++= (hoverOver(scaleViewToModel(awtShapeToJts(selectionRect))) filter { _._1 == 0 })
        }
        if (selectionPath != null) {
          selectionPath.closePath()
          selectedBlocks ++= (hoverOver(scaleViewToModel(awtShapeToJts(selectionPath))) filter { _._1 == 0})
        }
        selectionStart = null
        selectionPath = null
    }

    object UIState {
    }

    @volatile var battleModel: BattleModel = BattleModel(Seq(
      ShipInBattle(ShipMaker._ship,     mkPoint( 200, 200),       0), 
      ShipInBattle(ShipMaker.enemyShip, mkPoint( 600, 200), math.Pi)
    ))
    @volatile var mousePoint = new Point(0, 0)
    @volatile var selectionStart: Point = _
    @volatile var selectionPath: java.awt.geom.GeneralPath = _
    @volatile var selectedBlocks: Set[(Int, (Int, Int))] = Set() // ship -> pos
    @volatile var zoom = 1.0

    @volatile var speed = 1.0
    @volatile var paused = false

    val scale = new AffineTransformation()

    def selectionRect = if (selectionStart == null) new Rectangle() else {
      val sh = new Rectangle(selectionStart)
      sh.add(mousePoint)
      sh
    }

    def scaleModelToView(g: geom.Point): geom.Point       = scale.transform(g).asInstanceOf[geom.Point]
    def scaleModelToView(g: geom.Geometry): geom.Geometry = scale.transform(g)
    def scaleViewToModel(g: geom.Point): geom.Point       = scale.getInverse.transform(g).asInstanceOf[geom.Point]
    def scaleViewToModel(g: geom.Geometry): geom.Geometry = scale.getInverse.transform(g)

    // ship -> pos
    def hoverOverPoint(p: geom.Point): Option[(Int, (Int, Int))] = hoverOver(p).headOption
    def hoverOver(p: geom.Geometry): Seq[(Int, (Int, Int))] =
      (for {
        (sh, idx) <- battleModel.ships.zipWithIndex
        (blpos, geom) <- sh.blockGeom.toSeq
        if geom.contains(p) || p.contains(geom.getCentroid)
      } yield (idx, blpos))


    val targetFps = 30
    makeHeartBeatThread(targetFps) {
      panel.repaint()
      if (!paused) {
        val time = 1.0 / targetFps * speed
        battleModel = battleModel.tick(time)
      }
    }.start()

    override def paint(g: Graphics2D) {

      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

      def draw(color: Color = null, stroke: Double = 1, shape: Shape): Unit = {
        if (color != null) g.setColor(color)
        g.setStroke(new BasicStroke(stroke.toFloat/*, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND*/));
        g.draw(shape)
      }
      def fill(color: Color = null, shape: Shape): Unit = {
        if (color != null) g.setColor(color)
        g.fill(shape)
      }
      def color(r: Int, g: Int, b: Int, alpha: Int = 255) = new Color(r, g, b, alpha)

      val panelWidth  = size.getWidth.toInt
      val panelHeight = size.getHeight.toInt

      // draw stars
      g.setColor(Color.BLACK)
      g.fillRect(0, 0, panelWidth, panelHeight)

      /*
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
      */

      val installationColors: PartialFunction[Installation, Color] = {
        case NoInstallation  => Color.DARK_GRAY
        case i: PilotCapsule => Color.BLACK
        case i: EnergyCore   => Color.RED
        case i: Engine       => Color.BLUE
        case i: Storage      => Color.GREEN
        case i: Beam         => Color.YELLOW
      }

      // draw ships
      for (sib @ ShipInBattle(ship, pos, rot) <- battleModel.ships) {

        val hull = sib.geomColl.convexHull
        val a = hull.buffer(15)
        val b = hull.buffer(14.5)

        fill(color = new Color(173, 216, 230, 50),  shape = toAwtShape(scaleModelToView(a)))
        //draw(color = new Color(255, 255, 255, 180), shape = toAwtShape(scaleModelToView(b)))
        //draw(color = new Color(0x6495ed),           shape = toAwtShape(scaleModelToView(a)))

        for ((pos @ (x,y), geom) <- sib.blockGeom) {
          val block = sib.ship.blocks(pos)
          val col = installationColors(block.installation)
          fill(color = col,         shape = toAwtShape(scaleModelToView(geom)))
          draw(color = Color.WHITE, shape = toAwtShape(scaleModelToView(geom)))
        }

        g.setColor(Color.RED)
        val _pos = toAwtPoint(scaleModelToView(pos))
        drawCrosshair(g, _pos)
        //drawTooltip(g, s"$pos", _pos, true)
      }

      // draw lazors
      val lazors = for ((beamSrc @ (srcShipId, srcBlockPos), beamTarg @ (destShipId, destBlockPos)) <- battleModel.targeting) yield {
        val beam = battleModel.shipBlock(beamSrc).installation.asInstanceOf[Beam]
        val src  = battleModel.shipBlockGeom(beamSrc).getCentroid
        val targ = battleModel.shipBlockGeom(beamTarg).getCentroid
        val line = mkLine(src, targ)
        val hit = (line intersection battleModel.shipBlockGeom(battleModel.beamHits(beamSrc))).asInstanceOf[geom.LineString].getPointN(0) // todo, choose closer point
        val realLine = mkLine(src, hit)

        val lineToDraw = toAwtShape(scaleModelToView(realLine))

        val c = beam.color
        val outline = new Color(c.getRed, c.getBlue, c.getBlue, 150)

        (lineToDraw, outline, beam.color, Color.WHITE)
      }

      for ((lineToDraw, a, b, c) <- lazors) draw(color = a, stroke = 6 * zoom, shape = lineToDraw)
      for ((lineToDraw, a, b, c) <- lazors) draw(color = b, stroke = 4 * zoom, shape = lineToDraw)
      for ((lineToDraw, a, b, c) <- lazors) draw(color = c, stroke = 1 * zoom, shape = lineToDraw)

      // draw selected block
      val bls = for ((sh, blPos) <- selectedBlocks) yield battleModel.ships(sh).blockGeom(blPos)
      val blsc = mkGeometryCollection(bls.toArray)
      val bl = toAwtShape(scaleModelToView(blsc.buffer(8)))
      draw(color = Color.GREEN, shape = bl, stroke = 2)
      
      // draw selection rectangle
      draw(color = Color.WHITE, shape = selectionRect)

      // draw selection path
      if (selectionPath != null)
        draw(color = Color.WHITE, shape = selectionPath)

      // draw hover tooltip
      hoverOverPoint(scaleViewToModel(awtPointToJts(mousePoint))) match {
        case Some((sh, blpos)) =>
          val block = battleModel.ships(sh).ship.blocks(blpos)
          drawTooltip(g, ""+block, mousePoint, true)
        case _ =>
      }

      // draw speed and paused state
      val timeMsg = "speed: " + speed + (if (paused) " paused" else "")
      drawTooltip(g, timeMsg, new Point(10, 10))

      // draw destanation of ships
      for ((shipIdx, (pos, rot)) <- battleModel.movementDirections) {
        val _pos = toAwtPoint(scaleModelToView(pos))
        drawCrosshair(g, _pos)
        draw(color = Color.GREEN, shape = toAwtShape(scaleModelToView(mkLine(battleModel.ships(shipIdx).pos, pos))))
        draw(color = Color.GREEN, shape = toAwtShape(scaleModelToView(mkLine(pos, moveInDir(pos, 20, rot)))))
        //drawTooltip(g, ""+pos+" "+rot, _pos)
      }
    }
  }

//    contents = new BorderPanel {
//      layout += (shipPanel -> BorderPanel.Position.Center)
//      layout += (new Panel {} -> BorderPanel.Position.East)
//    }
    contents = battlePanel
    battlePanel.requestFocus()
  }

  def drawTooltip(g: Graphics2D, str: String, p: Point, upleft: Boolean = false) = {
    val lines = str.lines
    val metrics = g.getFontMetrics
    val lineHeight = metrics.getHeight
  
    for ((line, idx) <- lines.zipWithIndex) {
      val lineWidth = metrics.stringWidth(line)
  
      val boxw = lineWidth+10
      val boxh = lineHeight+5
  
      val xx = math.max(if (upleft) p.x - boxw else p.x, 0)
      val yy = math.max(if (upleft) p.y - boxh else p.y, 0) + (lineHeight + 5) * idx
  
      g.setColor(new Color(0,0,0,200))
      g.fillRect(xx, yy, boxw, boxh)
  
    //  g.setColor(Color.GRAY)
    //  g.drawRect(xx, yy, boxw, boxh)
  
      g.setColor(new Color(255,255,255, 200))
      g.drawString(line, xx+5, yy+lineHeight)
    }
  }
  
  def drawCrosshair(g: Graphics2D, p: Point) {
    g.drawLine(p.x-10, p.y,    p.x+10, p.y)
    g.drawLine(p.x,    p.y-10, p.x,    p.y+10)
    g.drawOval(p.x-10, p.y-10, 20, 20)
  }
}
