package com.github.scouto.sesion4

/**
  * Created by scouto.
  */
class Administracion (val relacionAlumnos: Map[Asignatura, List[Alumno]] = Map()) {


  /**
    * Debe dar de alta un alumno si no supera el máximo y el alumno no está ya presente
    * @param alumno
    * @param asignatura
    * @return
    */
  def alta(alumno: Alumno, asignatura: Asignatura): Option[Administracion] = {
        val alumnos = relacionAlumnos.getOrElse(asignatura, List())
        alumnos match {
          case Nil => Some(new Administracion(relacionAlumnos + (asignatura -> List(alumno))))
          case l if l.contains(alumno) => None
          case l if l.size < asignatura.plazas => Some(new Administracion(relacionAlumnos + (asignatura -> (alumno :: l))))
          case _ => None
        }

  }

  /**
    * Debe dar de baja un alumno o levantar un error si no es posible
    * @param alumno
    * @param asignatura
    * @return
    */
  def baja(alumno: Alumno, asignatura: Asignatura): Either[String, Administracion] = {
    var listaAlumnos = this.relacionAlumnos.getOrElse(asignatura, List())

    if (this.relacionAlumnos.get(asignatura) == Nil) {
      Left("Error: Asignatura sin alumnos")
    } else  if (listaAlumnos.contains(alumno)) {
      Right(new Administracion ( this.relacionAlumnos + (asignatura ->  (listaAlumnos.filter(_!=alumno)) )) )
    } else {
      Left("Alumno no inscrito")
    }
  }

  }


object Administracion {
  def apply(relacionAlumnos: Map[Asignatura, List[Alumno]] = Map()): Administracion = new Administracion(relacionAlumnos )

  def unapply(arg: Administracion): Option[(Map[Asignatura, List[Alumno]] )] = {
    Some((arg.relacionAlumnos))
  }
}
