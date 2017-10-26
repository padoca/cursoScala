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
    //var listaAlumnos = this.relacionAlumnos.get(asignatura).get //Falla si es None y no pasa los test
    var listaAlumnos = this.relacionAlumnos.getOrElse(asignatura, List())

    if (listaAlumnos == Nil) {
      Some( new Administracion (this.relacionAlumnos + (asignatura -> List(alumno))) )
    } else  if (listaAlumnos.contains(alumno)) {
      None
    } else if(asignatura.plazas > listaAlumnos.size) {
      Some (new Administracion (this.relacionAlumnos + (asignatura ->  (alumno::listaAlumnos) )) )
    } else {
      None
    }

    //Sol profe:
    //    val alumnos = relacionAlumnos.getOrElse(asignatura, List())
    //    alumnos match {
    //      case Nil => Some(new Administracion(relacionAlumnos + (asignatura -> List(alumno))))
    //      case l if l.contains(alumno) => None
    //      case l if l.size < asignatura.plazas => Some(new Administracion(relacionAlumnos + (asignatura -> (alumno :: l))))
    //      case _ => None
    //    }

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
