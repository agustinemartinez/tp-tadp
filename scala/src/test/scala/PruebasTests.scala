import TADPQuest.{CofreDeOro, Equipo, ForzarPuerta, Guerrero, Heroe, Inventario, Ladron, Mago, Mision, PelearContraMonstruo, RescatarPrincesa, Stats}
import org.scalatest.FlatSpec

class PruebasTests extends FlatSpec {
  def fixture = new {
    val mago: Heroe = Heroe(Stats(100, 30, 90, 120), Option(Mago)) // lider
    val guerrero: Heroe = Heroe(Stats(100, 110, 70, 50), Option(Guerrero))
    val ladron: Heroe = Heroe(Stats(100, 10, 50, 70), Option(Ladron), Inventario())
    val equipo: Equipo = Equipo("MagoYGuerrero", List(guerrero, mago))
    val mision = Mision(
      tareas = List(RescatarPrincesa, RescatarPrincesa, PelearContraMonstruo(30)),
      recompensa = CofreDeOro(1000)
    )
  }

  "Los efectos producidos por varias tareas en una misma misión" should "acumularse" in {
//    val equipoDeUno = Equipo("Solitario", List(fixture.ladron))
//    val misionParaUnoSolo = Mision(
//      List(PelearContraMonstruo(10), PelearContraMonstruo(20), ForzarPuerta, PelearContraMonstruo(50)),
//      CofreDeOro(5000)
//    )
//    val equipoDeUnoPostMision = equipoDeUno.prove(misionParaUnoSolo).get
//    val ladronPostMision = equipoDeUnoPostMision.integrantes(0)
//    println(ladronPostMision)
    val equipoPostMision = fixture.equipo.realizarMision(fixture.mision)
    println(equipoPostMision)
  }

}

