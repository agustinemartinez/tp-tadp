import TADPQuest._
import org.scalatest.FlatSpec

class CalculoDeFacilidadTests extends FlatSpec {
    def fixture = new {
      val guerrero: Heroe = Heroe(Stats(100, 150, 70, 50), Option(Guerrero), Inventario()) // Lider
      val ladron: Heroe = Heroe(Stats(100, 50, 90, 120), Option(Ladron), Inventario())
      val otroLadron: Heroe = Heroe(Stats(100, 40, 70, 90), Option(Ladron), Inventario())
      val equipo: Equipo = Equipo("GuerreroYDosLadrones", List(guerrero, ladron, otroLadron))
    }

    "Si el líder del equipo es un guerrero, la facilidad de éste para pelear contra " +
      "un monstruo" should "ser igual a 20" in {
      val facilidadLiderGuerrero = PelearContraMonstruo(5).facilidad(fixture.equipo.lider().get, fixture.equipo)
      assert(facilidadLiderGuerrero.equals(20))
    }

    "La facilidad para forzar una puerta " should "igual a la inteligencia del héroe más 10 por " +
      "cada ladrón en el equipo" in {
      val facilidadParaForzarPuerta = ForzarPuerta.facilidad(fixture.guerrero, fixture.equipo)
      // Inteligencia del heroe + 10 por cada ladron en el equipo
      //  - El guerrero tiene 50 de inteligencia inicial pero por tener como trabajo Guerrero se reduce a 40
      //  - Como hay dos ladrones en el equipo, entonces: 40 + 10 * 2 = 60
      assert(facilidadParaForzarPuerta.equals(60))
    }

    "La facilidad para robar un talisman" should "ser igual a la velocidad del heroe" in {
      val soloLadrones = Equipo("Ladrones", List(fixture.ladron, fixture.otroLadron))
      val liderLadron = soloLadrones.lider().get
      val facilidadParaRobarUnTalisman = RobarTalisman(TalismanMaldito).facilidad(liderLadron, soloLadrones)
      assert(facilidadParaRobarUnTalisman.equals(liderLadron.velocidad))
    }
}
