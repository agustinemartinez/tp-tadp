package TADPQuest

// Interfaz para modificador de stats
trait Modifier {
  def recalcularStats(stats: Stats, heroe: Heroe): Stats // Modificador que retorna nuevos Stats
}

// Enum para para los stats
trait Stat
object HP extends Stat
object Fuerza extends Stat
object Velocidad extends Stat
object Inteligencia extends Stat

case class Stats(hp: Int = 1, fuerza: Int = 1, velocidad: Int = 1, inteligencia: Int = 1) {
  require( hp > 0 , "La Stat HP no puede ser negativa" )
  require( fuerza > 0 , "La Stat Fuerza no puede ser negativa" )
  require( velocidad > 0 , "La Stat Velocidad no puede ser negativa" )
  require( inteligencia > 0 , "La Stat Inteligencia no puede ser negativa" )

  def valor(stat: Stat): Int = stat match {
    case HP => hp
    case Fuerza => fuerza
    case Velocidad => velocidad
    case Inteligencia => inteligencia
  }
}