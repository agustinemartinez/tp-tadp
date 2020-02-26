package TADPQuest

trait Item extends Modifier {
  def cumpleRestriccion(heroe: Heroe): Boolean = true // Por defecto, todos los items se pueden equipar
  def valor(): Int = 1 // Por defecto los items valen 1
}

// Items
trait Casco    extends Item
trait Armadura extends Item
trait Arma     extends Item {
  def requiereDosManos(): Boolean
}
trait Talisman extends Item

// Null-Object Pattern
case class Vacio() extends Item {
  override def cumpleRestriccion(heroe: Heroe): Boolean = true
  override def valor(): Int = 0
  override def recalcularStats(stats: Stats, heroe: Heroe): Stats = stats
}
object CascoVacio    extends Vacio with Casco
object ArmaduraVacio extends Vacio with Armadura
object ArmaVacio     extends Vacio with Arma {
  override def requiereDosManos(): Boolean = false
}

case class Inventario(
                       cabeza: Casco = CascoVacio,
                       armadura: Armadura = ArmaduraVacio,
                       manos: (Arma, Arma) = (ArmaVacio, ArmaVacio),
                       talismanes: List[Talisman] = List()
                     ) extends Modifier {

  def recalcularStats(stats: Stats, heroe: Heroe): Stats = {
    // Modifica los stats base aplicando los modificadores de cada item con fold
    // Si el item es Vacio, las stats no sufren efecto y se continua iterando
    val items: List[Item] = List(cabeza, armadura) ++ armasEquipadas() ++ talismanes
    items.foldLeft(stats) { (accum, item) => item.recalcularStats(accum, heroe) }
  }

  def agregarItem(item: Item): Inventario = item match {
    // Asigna el item segun su tipo
    case item: Casco    => copy(cabeza = item)
    case item: Armadura => copy(armadura = item)
    case item: Arma     => asignarArma(item)
    case item: Talisman => copy(talismanes = item :: talismanes)
    case _ => this
  }

  private def asignarArma(nuevaArma: Arma): Inventario = manos match {
    // Si la nueva arma requiere las dos manos, asigno en las dos
    case _ if nuevaArma.requiereDosManos() => copy(manos = (nuevaArma, nuevaArma))
    // Si no hay armas, asigno en la izq
    case (ArmaVacio, ArmaVacio) => copy(manos = (nuevaArma, ArmaVacio))
    // Si solo hay un arma en una mano, asigno en la que esta libre
    case (ArmaVacio, armaActual) => copy(manos = (nuevaArma, armaActual))
    case (armaActual, ArmaVacio) => copy(manos = (armaActual, nuevaArma))
    // Si tengo un arma de dos manos, la tiro y asigno la nueva en la mano izq
    case (armaActual, _) if armaActual.requiereDosManos() => copy(manos = (nuevaArma, ArmaVacio))
    // Si tengo dos armas que ocupan una mano, tiro el arma de la mano izq y asigno la nueva
    case (_, armaActual) => copy(manos = (nuevaArma, armaActual))
  }

  def armasEquipadas(): List[Arma] = {
    if (manos._1.requiereDosManos()) List(manos._1)
    else List(manos._1, manos._2)
  }

  def cantidadDeItems: Int = {
    val items = List(cabeza, armadura) ++ armasEquipadas() ++ talismanes
    items.filterNot(_.isInstanceOf[Vacio]).size
  }
}

/************ ITEMS (Dominio) *************/

// --- CASCOS/SOMBREROS ---
object CascoVikingo extends Casco {
  override def recalcularStats(stats: Stats, heroe: Heroe): Stats = stats.copy(hp = stats.hp + 10)
  override def cumpleRestriccion(heroe: Heroe): Boolean = heroe.baseStats.fuerza > 30
}

object VinchaDelBufaloDeAgua extends Casco {
  override def recalcularStats(stats: Stats, heroe: Heroe): Stats = {
    if (heroe.stats.fuerza > heroe.stats.inteligencia)
      stats.copy(inteligencia = stats.inteligencia + 30)
    else stats.copy(
      hp = stats.hp + 10,
      fuerza = stats.hp + 10,
      velocidad = stats.velocidad + 10
    )
  }
  override def cumpleRestriccion(heroe: Heroe): Boolean = heroe.trabajo.isEmpty
}

// --- VESTIDOS/ARMADURAS ---
object ArmaduraEleganteSport extends Armadura {
  override def recalcularStats(stats: Stats, heroe: Heroe): Stats =
    stats.copy(hp = stats.hp - 30, velocidad = stats.velocidad + 30)
}

// --- ARMAS/ESCUDOS ---
object ArcoViejo extends Arma {
  override def recalcularStats(stats: Stats, heroe: Heroe): Stats = stats.copy(fuerza = stats.fuerza + 2)
  override def requiereDosManos(): Boolean = true
}

object EspadaDeLaVida extends Arma {
  override def recalcularStats(stats: Stats, heroe: Heroe): Stats = stats.copy(fuerza = stats.hp)
  override def requiereDosManos(): Boolean = false
}

object EscudoAntiRobo extends Arma {
  override def recalcularStats(stats: Stats, heroe: Heroe): Stats = stats.copy(hp = stats.hp + 20)
  override def cumpleRestriccion(heroe: Heroe): Boolean =
    heroe.trabajo match {
      case Some(Ladron) => false
      case Some(_) if heroe.baseStats.fuerza < 20 => false
      case _ => true // Incluye el caso de que no tenga trabajo (permite equiparlo)
    }
  override def requiereDosManos(): Boolean = false
}

object PalitoMagico extends Arma {
  override def recalcularStats(stats: Stats, heroe: Heroe): Stats =
    stats.copy(inteligencia = stats.inteligencia + 20)
  override def cumpleRestriccion(heroe: Heroe): Boolean = heroe.trabajo match {
    case Some(Mago) => true
    case Some(Ladron) if heroe.baseStats.inteligencia > 30 => true
    case _ => false
  }
  override def requiereDosManos(): Boolean = false
}

// --- TALISMANES ----
object TalismanDeDedicacion extends Talisman {
  override def recalcularStats(stats: Stats, heroe: Heroe): Stats = {
    val incremento = (heroe.valorStatPrincipal() * 0.1).toInt
    stats.copy(
      hp = stats.hp + incremento ,
      fuerza = stats.fuerza + incremento,
      velocidad = stats.velocidad + incremento,
      inteligencia = stats.inteligencia + incremento
    )
  }
}

object TalismanDelMinimalismo extends Talisman {
  override def recalcularStats(stats: Stats, heroe: Heroe): Stats = {
    val hpARestar = (heroe.cantidadItemsEquipados - 1) * 10
    stats.copy(hp = stats.hp + 50 - hpARestar)
  }
}

object TalismanMaldito extends Talisman {
  override def recalcularStats(stats: Stats, heroe: Heroe): Stats = Stats()
}