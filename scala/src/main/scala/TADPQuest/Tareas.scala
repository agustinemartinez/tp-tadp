package TADPQuest

trait Tarea {
  def facilidad(heroe: Heroe, equipo: Equipo): Int
  // Me devuelve un nuevo heroe con los efectos producidos por realizar la tarea
  def serRealizadaPor(heroe: Heroe): Heroe
}

/*** TAREAS (DOMINIO) ***/

case class PelearContraMonstruo(hpAReducir: Int) extends Tarea {
  override def facilidad(heroe: Heroe, equipo: Equipo): Int = {
    heroe.trabajo match {
      case Some(Guerrero) if equipo.lider().contains(heroe) => 20
      case _ => 10
    }
  }

  override def serRealizadaPor(heroe: Heroe): Heroe = {
    heroe.stats.fuerza match {
      case valorDeFuerza if valorDeFuerza < 20 =>
        val nuevosStats = heroe.baseStats.copy(hp = heroe.baseStats.hp - hpAReducir)
        heroe.copy(baseStats = nuevosStats)
      case _ => heroe
    }
  }
}

case object ForzarPuerta extends Tarea {
  override def facilidad(heroe: Heroe, equipo: Equipo): Int = {
    val cantidadDeLadrones = equipo.integrantes.map(_.trabajo).map {
      case Some(Ladron) => 1
      case _ => 0
    }.sum
    heroe.stats.inteligencia + 10 * cantidadDeLadrones
  }

  override def serRealizadaPor(heroe: Heroe): Heroe = {
    heroe.trabajo match {
      case Some(Mago) | Some(Ladron) => heroe
      case _ =>
        val nuevosStats = heroe.baseStats.copy(hp = heroe.baseStats.hp - 5, fuerza = heroe.baseStats.fuerza + 1)
        heroe.copy(baseStats = nuevosStats)
    }
  }
}

case class RobarTalisman(talisman: Talisman) extends Tarea {
  override def facilidad(heroe: Heroe, equipo: Equipo): Int = {
    equipo.lider() match {
      case Some(Heroe(_,Some(Ladron),_)) => heroe.stats.velocidad
      case _ => throw NoSePuedeRealizarTareaException(this)
    }
  }

  override def serRealizadaPor(heroe: Heroe): Heroe = {
    val nuevoInventario: Inventario = heroe.inventario.agregarItem(talisman)
    heroe.copy(inventario = nuevoInventario)
  }
}

// Agrego una tarea nueva para probar la extensibilidad de la solución
case object RescatarPrincesa extends Tarea {
  override def facilidad(heroe: Heroe, equipo: Equipo): Int = {
    heroe.trabajo match {
      case Some(Guerrero) if heroe.fuerza > 100 => 50
      case Some(Ladron) if heroe.velocidad > 70 => 30
      case _ => equipo.lider().map(_.inteligencia).getOrElse(1) max 20
    }
  }

  override def serRealizadaPor(heroe: Heroe): Heroe = {
    val nuevosStats = heroe.baseStats.copy(
      hp = heroe.baseStats.hp + 150,
      inteligencia = heroe.baseStats.inteligencia * 2)
    heroe.copy(baseStats = nuevosStats)
  }
}
