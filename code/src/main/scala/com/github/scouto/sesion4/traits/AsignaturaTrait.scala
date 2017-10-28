package com.github.scouto.sesion4.traits

trait AsignaturaTrait {
  val nombre: String
  val plazas: Int=30
  val descripcion: Option[String] = None

  def prioridad: Boolean
}
