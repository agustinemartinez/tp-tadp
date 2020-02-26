import TADPQuest._
import org.scalatest.FlatSpec

import scala.util.Failure

class MisionesTests extends FlatSpec {
  def fixture = new {
    val mago: Heroe = Heroe(Stats(100, 30, 90, 120), Option(Mago)) // lider
    val guerrero: Heroe = Heroe(Stats(100, 110, 70, 50), Option(Guerrero))
    val ladron: Heroe = Heroe(Stats(100, 10, 50, 70), Option(Ladron), Inventario())
    val equipo: Equipo = Equipo("MagoYGuerrero", List(mago, guerrero))
    val mision = Mision(
      tareas = List(PelearContraMonstruo(50), ForzarPuerta),
      recompensa = CofreDeOro(1000)
    )
  }

  "Un integrante de un equipo" should "verse afectado al realizar una misión" in {
    // Ambas tareas son realizadas por el mago:
    //  - PelearContraMonstruo le resta los 50 de hp que pasamos por parámetro
    //  - ForzarPuerta no tiene efecto contra los magos
    val equipoDespuesDeMision = fixture.equipo.realizarMision(fixture.mision)
    val hpMagoAntesDeMision = fixture.mago.stats.hp
    val hpMagoDespuesDeMision = equipoDespuesDeMision.get.integrantes(0).hp
    assert(hpMagoAntesDeMision.equals(hpMagoDespuesDeMision + 50))
  }

  "Al realizar una mision exitosamente, el equipo" should "cobrar la recompensa" in {
    val pozoComunAntesDeMision = fixture.equipo.pozoComun
    val pozoComunDespuesDeMision = fixture.equipo.realizarMision(fixture.mision).get.pozoComun
    assert(pozoComunDespuesDeMision.equals(pozoComunAntesDeMision + 1000)) // La recompensa son 1000 de oro
  }

  "Si un equipo no puede realizar una tarea de la misión, la misión entera" should "fallar" in {
    // No se puede realizar la misión porque se necesita que el líder del equipo sea un ladrón para
    // robar un talismán (y el líder es un mago)
    val misionImposible = fixture.mision.copy(tareas = RobarTalisman(TalismanMaldito) :: fixture.mision.tareas)
    //assert(fixture.equipo.realizarMision(misionImposible).equals(Failure(NoSePuedeRealizarTareaException(RobarTalisman(TalismanMaldito)))))
    assert(fixture.equipo.realizarMision(misionImposible).isFailure)
  }

  "Al robar un talisman" should "agregar el talisman robado a un integrante del equipo"

  "Los efectos producidos por varias tareas en una misma misión" should "acumularse" in {
    val equipoDeUno = Equipo("Solitario", List(fixture.ladron))
    val misionParaUnoSolo = Mision(
      List(PelearContraMonstruo(10), PelearContraMonstruo(20), ForzarPuerta, PelearContraMonstruo(50)),
      CofreDeOro(5000)
    )
    val equipoDeUnoPostMision = equipoDeUno.realizarMision(misionParaUnoSolo).get
    val ladronDespuesDeMision = equipoDeUnoPostMision.integrantes(0)
    // ladron arranca con 100 de hp pero se le reduce 5 por tener trabajo Ladrón -> 95
    // Pelear contra tres monstruos que le quitan 10 + 20 + 50 = 80 puntos de hp -> 95 - 80 = 15
    assert(ladronDespuesDeMision.hp.equals(15))
  }
}