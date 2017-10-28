package com.github.scouto.sesion4.traits

case class AlumnoRepetidor ( nombre: String, apellidos: String) extends AlumnoTrait {
  override def repetidor: Boolean = true
}
