package ssoss

import language.postfixOps

object ShipMaker {

  val makeBlock = Map[Char, Block](
    '#' -> Block(material = Material.skeleton, installation = PilotCapsule())
  , 'P' -> Block(material = Material.titanium, installation = NoInstallation)
  , 'E' -> Block(material = Material.skeleton, installation = EnergyCore(100))
  , '=' -> Block(material = Material.skeleton, installation = Engine(100, 500000))
  , 'L' -> Block(material = Material.skeleton, installation = Beam.laser)
  , 'p' -> Block(material = Material.skeleton, installation = Beam.phaser)
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
     =P     L
    =PPL    LL
  ===E#PPPPPPPPp
    =PPL    LL
     =P     L
  """)

  val enemyShip = makeShip("""
  PPPPPPP
      PPPP
       PPPPP
      PPPP
  PPPPPPP
  """)

}
