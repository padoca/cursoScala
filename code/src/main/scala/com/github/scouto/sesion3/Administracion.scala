package com.github.scouto.sesion3

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
    var listaAlumnos = this.relacionAlumnos.get(asignatura).get

    if (this.relacionAlumnos.get(asignatura) == Nil) {
      Some( new Administracion (this.relacionAlumnos + (asignatura -> List(alumno))) )
    } else  if (this.relacionAlumnos.get(asignatura).contains(alumno)) {
      None
    } else if(asignatura.limite > this.relacionAlumnos.get(asignatura).size) {
      Some (new Administracion (this.relacionAlumnos + (asignatura ->  (alumno::listaAlumnos) )) )
    } else {
      None
    }

  }

  /**
    * Debe dar de baja un alumno o levantar un error si no es posible
    * @param alumno
    * @param asignatura
    * @return
    */
  def baja(alumno: Alumno, asignatura: Asignatura): Either[String, Administracion] = {
    var listaAlumnos = this.relacionAlumnos.get(asignatura).get

    if (this.relacionAlumnos.get(asignatura) == Nil) {
      Left("Error: Asignatura sin alumnos")
    } else  if (this.relacionAlumnos.get(asignatura).contains(alumno)) {
      Right(new Administracion ( this.relacionAlumnos + (asignatura ->  (listaAlumnos.filter(_!=alumno)) )) )
    } else {
      Left("Error: Alumno no encontrado")
    }
  }

  }
