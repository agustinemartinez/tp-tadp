package TADPQuest

object Taberna {
  val misionPeligrosa = Mision(
    List(PelearContraMonstruo(20), PelearContraMonstruo(30)),
    CofreDeOro(1000)
  )
  // Otras misiones para tests
  val misionMasPeligrosa = Mision(
    List(PelearContraMonstruo(40), ForzarPuerta, PelearContraMonstruo(50)),
    CofreDeOro(2000)
  )
  val misionLarga = Mision(
    List(PelearContraMonstruo(10), ForzarPuerta, PelearContraMonstruo(40)),
    NuevoHeroe(Heroe(Stats(100, 50, 60, 120), Option(Mago), Inventario()))
  )
  val misionFuerzaParaLosMagos = Mision(
    List(ForzarPuerta, RescatarPrincesa),
    IncrementarFuerzaALosMagos(200)
  )
  val misionParaLadronLider = Mision(
    List(ForzarPuerta, RobarTalisman(TalismanMaldito)),
    NuevoItem(TalismanMaldito)
  )

  // Tablon de misiones
  val tablon: List[Mision] = List(
    misionPeligrosa, misionFuerzaParaLosMagos, misionParaLadronLider, misionLarga
  )
}