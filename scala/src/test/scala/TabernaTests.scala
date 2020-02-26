import TADPQuest.Taberna._
import TADPQuest._
import org.scalatest.FlatSpec

import scala.util.Failure

class TabernaTests extends FlatSpec {
  def fixture = new {
    val mago: Heroe = Heroe(Stats(1000, 30, 90, 120), Option(Mago))
    val guerrero: Heroe = Heroe(Stats(1000, 110, 70, 50), Option(Guerrero))
    val ladron: Heroe = Heroe(Stats(1000, 10, 200, 70), Option(Ladron), Inventario()) // lider
    val equipo: Equipo = Equipo("MagoYGuerreroYLadron", List(mago, guerrero, ladron))
  }

  "Cuando el criterio es el de mayor pozo comun se" should "elegir la misionPeligrosa ya" +
    "que tiene recompensa de 1000 de oro (es la que mas oro da del tablon)" in {
    val misionElegida: Mision = fixture.equipo.elegirMision((e1, e2) => e1.pozoComun > e2.pozoComun, tablon).get
    assert(misionElegida.equals(misionPeligrosa))
  }

  "Cuando el criterio es el de la misión que mayor fuerza agrega a los magos se" should "elegir" +
    "la misionFuerzaParaLosMagos" in {
    val misionElegida: Mision = fixture.equipo.elegirMision(
      (e1, e2) => e1.integrantesQueTrabajanComo(Mago)(0).fuerza > e2.integrantesQueTrabajanComo(Mago)(0).fuerza,
      tablon
    ).get
    assert(misionElegida.equals(misionFuerzaParaLosMagos))
  }

  "Si el entrenamiento es exitoso se" should "acumular las recompensas cobradas" in {
    val guerreroLider = Heroe(Stats(1000, 250, 50, 60), Option(Guerrero), Inventario())
    val equipoConGuerreroLider = fixture.equipo.obtenerMiembro(guerreroLider)
    val equipoEntrenado = equipoConGuerreroLider.entrenar(
      (e1, e2) => e1.pozoComun > e2.pozoComun,
      List(misionPeligrosa, misionLarga, misionMasPeligrosa)
    )
    // misionPeligrosa da 1000 de oro y misionMasPeligrosa da 2000 de oro
    // misionLarga agrega un nuevo miembro, no da oro => 3000 de oro después de entrenar
    assert(equipoEntrenado.get.pozoComun.equals(3000))
  }

  "Si el entrenamiento incluye una mision que el equipo no puede realizar, el entrenamiento" should "fallar" in {
    val guerreroLider = Heroe(Stats(1000, 250, 50, 60), Option(Guerrero), Inventario())
    val equipoConGuerreroLider = fixture.equipo.obtenerMiembro(guerreroLider)
    val equipoEntrenado = equipoConGuerreroLider.entrenar( (e1, e2) => e1.pozoComun > e2.pozoComun, tablon )
    // Como el equipo no puede realizar la tarea "RobarTalisman" de la mision "misionParaLadronLider"
    // no se elige esa mision y retorna Failture
    assert( equipoEntrenado.equals( Failure(NoSeEligioMisionException()) ) )
    //assert( equipoEntrenado.equals( Failure(NoSePuedeRealizarTareaException(RobarTalisman(TalismanMaldito))) ) )
  }

}
