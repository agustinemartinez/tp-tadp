import TADPQuest._
import org.scalatest.FlatSpec

class SeleccionDeHeroeTests extends FlatSpec {
  def fixture = new {
    val guerrero: Heroe = Heroe(Stats(100, 150, 70, 50), Option(Guerrero), Inventario()) // Lider
    val mago: Heroe = Heroe(Stats(100, 50, 90, 120), Option(Mago), Inventario())
    val equipo: Equipo = Equipo("GuerreroYMago", List(guerrero, mago))
  }

  "Si hay un guerrero en el equipo" should "elegirlo para pelear contra un monstruo" in {
    val heroeElegido = fixture.equipo.elegirHeroePara(PelearContraMonstruo(5)).get
    assert(fixture.guerrero.equals(heroeElegido))
  }

  "Si el lider del equipo no es un ladrón no" should "elegirse ningún heroe para robar un talisman y fallar" in {
    val heroeElegido = fixture.equipo.elegirHeroePara(RobarTalisman(TalismanMaldito))
    assert(heroeElegido.isFailure)
  }

  "En caso de empate" should "elegirse al primer heroe de la lista de integrantes" in {
    // otroMago tiene los mismos valores que mago (inteligencia = 120)
    val primerMago: Heroe = Heroe(Stats(100, 50, 90, 120), Option(Mago), Inventario())
    val equipoConOtroMago = Equipo("SoloMagos", List(primerMago, fixture.mago))
    val heroeElegido = equipoConOtroMago.elegirHeroePara(ForzarPuerta).get
    // Como los dos magos tiene 120 de inteligencia hay empate
    // Se elige al primerMago porque se ubica primero en la lista
    assert(heroeElegido.equals(primerMago))
  }
}