package com.github.scouto.sesion4.traits

case class AsignaturaConPrioridad(nombre: String, override val plazas: Int=30, override val descripcion: Option[String] = None) extends AsignaturaTrait {
  override def prioridad = true
}
