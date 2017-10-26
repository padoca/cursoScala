package com.github.scouto.sesion4

class Persona (val nombre: String, val apellidos: String, val edad: Int) {

}

object Persona {
  def apply( nombre: String,   apellidos: String,   edad: Int): Persona = new Persona( nombre,  apellidos,  edad  )

  def unapply(arg: Persona): Option[(String, String, Int)] = {
    Some((arg.nombre,arg.apellidos,arg.edad))
  }
}
