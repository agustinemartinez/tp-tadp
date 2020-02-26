import org.scalatest.{FlatSpec, FunSpec}
import scala.util.{Success, Failure, Try}
import TADPQuest._

class InventarioYStatsTests extends FlatSpec {

  def fixture = new {
    val baseStats: Stats = Stats(100,100,100,100)
    val guerreroSimple: Heroe = Heroe(baseStats, Option(Guerrero), Inventario())
    val guerreroConCascoYArmadura: Heroe = Heroe(baseStats, Option(Guerrero),
      Inventario(cabeza = CascoVikingo, armadura = ArmaduraEleganteSport))
    val ladronSimple: Heroe = Heroe(baseStats, Option(Ladron), Inventario())

    val equipo: Equipo = Equipo("Los Patos Salvajes", List(
      guerreroSimple,
      guerreroConCascoYArmadura,
      ladronSimple
    ))
  }

  "Un guerrero simple" should "modificar sus stats finales solo por el trabajo" in {
    val guerreroSimpleStats = fixture.guerreroSimple.stats
    val expectedStats = Guerrero.recalcularStats(fixture.baseStats, fixture.guerreroSimple)
    assert(guerreroSimpleStats.equals(expectedStats))
  }

  "Un guerrero simple" should "poder cambiar de trabajo y obtener nuevas stats finales" in {
    val magoSimple = fixture.guerreroSimple.cambiarTrabajo(Option(Mago))
    val magoSimpleStats = magoSimple.stats
    val expectedStats = Mago.recalcularStats(fixture.baseStats, magoSimple)
    assert(magoSimpleStats.equals(expectedStats))
  }

  "Un guerrero simple" should "poder agregar nuevos items y modificar sus stats finales" in {
    val guerreroConItems = fixture.guerreroSimple
      .agregarItem(CascoVikingo)
      .agregarItem(ArmaduraEleganteSport)
      .agregarItem(ArcoViejo)
    val actualStats = guerreroConItems.stats
    val expectedStats = Stats(90, 117, 130, 90)
    assert(actualStats.equals(expectedStats))
  }

  "Un arma de dos manos" should "ser la unica arma del guerrero" in {
    val guerreroConDosArmas = fixture.guerreroSimple
      .agregarItem(EspadaDeLaVida)
      .agregarItem(EscudoAntiRobo)
    assert( guerreroConDosArmas.inventario.manos.equals( (EspadaDeLaVida, EscudoAntiRobo) ) )

    val guerreroConUnArmaDeDosManos = guerreroConDosArmas.agregarItem(ArcoViejo)
    assert( guerreroConUnArmaDeDosManos.inventario.manos.equals( (ArcoViejo, ArcoViejo) ) )

    val guerreroConUnArma = guerreroConUnArmaDeDosManos.agregarItem(EspadaDeLaVida)
    assert( guerreroConUnArma.inventario.manos.equals( (EspadaDeLaVida, ArmaVacio) ) )
  }

  "Si tengo dos armas que ocupan una mano y me llega otra arma nueva de una mano" should "tirar solo un arma" in {
    val guerreroConDosArmas = fixture.guerreroSimple
      .agregarItem(EspadaDeLaVida)
      .agregarItem(EscudoAntiRobo)
    assert( guerreroConDosArmas.inventario.manos.equals( (EspadaDeLaVida, EscudoAntiRobo) ) )

    val guerreroConDosEscudos = guerreroConDosArmas.agregarItem(EscudoAntiRobo)
    assert( guerreroConDosEscudos.inventario.manos.equals( (EscudoAntiRobo, EscudoAntiRobo) ) )
  }

  "Un item que no cumple las restricciones" should "no ser agregado al inventario del heroe" in {
    val guerreroSinArmas = fixture.guerreroSimple
      .agregarItem(PalitoMagico)
      .agregarItem(EspadaDeLaVida)
    assert( guerreroSinArmas.inventario.manos.equals( (EspadaDeLaVida, ArmaVacio) ) )
  }

  "El criterio de mas veloz" should "devolver el heroe con mayor velocidad del equipo" in {
    val criterioMasVeloz: Heroe => Int = _.stats.velocidad
    val heroeMasVeloz = fixture.equipo.mejorHeroeSegun(criterioMasVeloz).get
    assert( heroeMasVeloz.equals(fixture.guerreroConCascoYArmadura) )
  }

  "El criterio de mas veloz" should "devolver None si el equipo esta vacio" in {
    val equipoVacio = Equipo("Equipo vacio")
    val criterioMasVeloz: Heroe => Int = _.stats.velocidad
    val heroeMasVeloz = equipoVacio.mejorHeroeSegun(criterioMasVeloz)
    assert( heroeMasVeloz.isEmpty )
  }

  "Cuando se agrega un nuevo heroe al equipo" should "agregarlo a la lista de integrantes" in {
    val equipoVacio = Equipo("Equipo vacio")
    val equipoConIntegrante = equipoVacio.obtenerMiembro(fixture.guerreroSimple)
    assert( equipoConIntegrante.integrantes.equals( List(fixture.guerreroSimple) ) )
  }

  "Cuando se reemplaza un heroe por otro en un equipo" should "reemplazarlo en la lista de integrantes" in {
    val otroGuerreroSimple = fixture.guerreroSimple.copy()
    val equipoConMiembroRemplazado = fixture.equipo.reemplazarMiembro(fixture.ladronSimple, otroGuerreroSimple)
    val listaNuevosIntegrantes = List(
      fixture.guerreroSimple,
      fixture.guerreroConCascoYArmadura,
      otroGuerreroSimple
    )
    assert(equipoConMiembroRemplazado.integrantes.equals(listaNuevosIntegrantes))
  }

  "El líder de un equipo" should "ser el heroe con el mayor valor en su stat principal" in {
    val equipoConUnLider = Equipo("Equipo con un lider", List(
      fixture.ladronSimple,
      fixture.ladronSimple.copy(),
      fixture.guerreroSimple
    ))
    val lider = equipoConUnLider.lider().get
    assert(lider.equals(fixture.guerreroSimple))
  }

  "El líder de un equipo" should "ser None si el equipo no tiene integrantes o hay mas de un lider" in {
    val equipoVacio = Equipo("Equipo vacio")
    val liderEquipoVacio = equipoVacio.lider()
    assert(liderEquipoVacio.isEmpty)

    val lider = fixture.equipo.lider()
    assert(lider.isEmpty)
  }

  "Cuando un equipo obtiene un item" should "ser agregado al heroe al que le produzca el " +
    "mayor incremento en la main stat de su job" in {
    val equipoQueObtieneItem = fixture.equipo.obtenerItem(ArmaduraEleganteSport)
    val integranteQueObtieneItem = fixture.ladronSimple.agregarItem(ArmaduraEleganteSport)
    assert(equipoQueObtieneItem.integrantes.contains(integranteQueObtieneItem))
  }

  "Cuando un equipo obtiene un item" should "ser vendido si ningun heroe consigue aumentar su stat principal" in {
    val equipoQueObtieneItem = fixture.equipo.obtenerItem(PalitoMagico)
    assert(equipoQueObtieneItem.integrantes.equals(fixture.equipo.integrantes))
    assert(equipoQueObtieneItem.pozoComun.equals(1))
  }
}




