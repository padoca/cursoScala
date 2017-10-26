package com.github.scouto.sesion4

class Asignatura (val nombre: String, val plazas: Int=30, val descripcion: Option[String] = None)

object Asignatura {
  def apply(nombre: String, plazas: Int=30, descripcion: Option[String] = None): Asignatura = new Asignatura(nombre, plazas, descripcion)
  def unapply(arg: Asignatura): Option[(String, Int, Option[String])] = {
    Some((arg.nombre, arg.plazas,arg.descripcion))
  }
}
