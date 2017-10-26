package com.github.scouto.sesion4

class Alumno (val nombre: String, val apellidos: String)

object Alumno {
  def apply( nombre: String, apellidos: String): Alumno = new Alumno(nombre, apellidos)

  def unapply(arg: Alumno): Option[(String, String)] = {
    Some((arg.nombre,arg.apellidos))
  }
}

