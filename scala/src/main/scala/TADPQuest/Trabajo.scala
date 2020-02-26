package TADPQuest

trait Trabajo extends Modifier {
  def statPrincipal(): Stat
  def cobra(): Int = 0 // Por defecto trabajan gratis
}

/*** TRABAJOS (DOMINIO) ***/

trait Luchador extends Trabajo {
  override def statPrincipal(): Stat = Fuerza
  override def recalcularStats(stats: Stats, heroe: Heroe): Stats =
    Stats(stats.hp + 10, stats.fuerza + 15, stats.velocidad, stats.inteligencia - 10)
}

object Guerrero extends Luchador

case class Mercenario(override val cobra: Int) extends Luchador

object Mago extends Trabajo {
  override def statPrincipal(): Stat = Inteligencia
  override def recalcularStats(stats: Stats, heroe: Heroe): Stats =
    Stats(stats.hp, stats.fuerza - 20, stats.velocidad, stats.inteligencia + 20)
}

object Ladron extends Trabajo {
  override def statPrincipal(): Stat = Velocidad
  override def recalcularStats(stats: Stats, heroe: Heroe): Stats =
    Stats(stats.hp - 5, stats.fuerza, stats.velocidad + 10, stats.inteligencia)
}


// Concatenar listas:                 list ++ otherList
// Agregar elemento a lista adelante: element :: list
// Agregar elemento a lista atras:    list ++ List(element)
// Quitar elemento de una lista:      list.diff(List(element))
// Ordenar de menor a mayor:          lista.sortBy( (TipoLista) => Int )

// Para elegir el "mejor" de algo:
// Si la lista esta vacia, devuelve la semilla (None/Failture(Error))
// lista.foldLeft(semilla) { (acumulado, siguienteElemento) =>
//  procesar(siguienteElemento) match {
//    case Success/Some if acumulado.isEmpty | esMejor => acumuladoMejorado
//    case _ => acumulado
//  }
// }

// Para filtrar y mapear con collect:
// lista.collect { case elemento @ Heroe(_,Some(Mercenario(costo)),_) if costo < 10 => heroe }

// Option/Try(value).map( _.do() ) me retorna un Option/Try mapeado
// Option/Try(Option/Try(value)).flatMap( a => a.map(_.do()) ) me retorna un Option/Try mapeado
// (Try/Option).getOrElse(...)
// Try(1/0).toOption -> None
// No se puede heredar de una case class, en su lugar hacer un trait
// con el comportamiento y que lo implementen ambas clases
// List().head -> NoSuchElementException

