package com.github.scouto.sesion4.traits

case class AlumnoNuevo ( nombre: String, apellidos: String) extends AlumnoTrait {
  override def repetidor: Boolean = false
}
