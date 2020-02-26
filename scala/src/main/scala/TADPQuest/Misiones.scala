package TADPQuest

case class Mision(tareas: List[Tarea], recompensa: Recompensa)

trait Recompensa
case class CofreDeOro(cantidadDeOro: Int) extends Recompensa
case class NuevoItem(item: Item) extends Recompensa
case class NuevoHeroe(heroe: Heroe) extends Recompensa
case class IncrementarFuerzaALosMagos(incremento: Int) extends Recompensa
