package com.github.scouto.sesion4.traits


class AdministracionTrait  (val relacionAlumnos: Map[AsignaturaTrait, List[AlumnoTrait]] = Map()) {

  def baja(alumno: AlumnoTrait, asignatura: AsignaturaTrait): Either[String, AdministracionTrait] = {
    val alumnos = relacionAlumnos.getOrElse(asignatura, List())

    alumnos match {
      case Nil => Left("Alumno no inscrito")
      case l if l.contains(alumno) => Right(new AdministracionTrait(relacionAlumnos + (asignatura -> alumnos.filterNot(_ == alumno))))
      case _ => Left("Alumno no inscrito")
    }
  }

  //Alta generico
  def alta(alumno: AlumnoTrait, asignatura: AsignaturaTrait): Option[AdministracionTrait] = {
    (asignatura, alumno) match {
      case (AsignaturaConPrioridad(_, _, _), AlumnoNuevo(_, _)) => altaConPrioridad(alumno, asignatura)
      case _ => altaSinPrioridad(alumno, asignatura)
    }
  }

  def altaSinPrioridad(alumno: AlumnoTrait, asignatura: AsignaturaTrait): Option[AdministracionTrait] = {
    val alumnos = relacionAlumnos.getOrElse(asignatura, List())
    alumnos match {
      case Nil => Some(new AdministracionTrait(relacionAlumnos + (asignatura -> List(alumno))))
      case l if l.contains(alumno) => None
      case l if l.size < asignatura.plazas => Some(new AdministracionTrait(relacionAlumnos + (asignatura -> (alumno :: l))))
      case _ => None
    }
  }

  def altaConPrioridad(alumno: AlumnoTrait, asignatura: AsignaturaTrait): Option[AdministracionTrait] = {
    val alumnos = relacionAlumnos.getOrElse(asignatura, List())
    alumnos match {
      case Nil => Some(new AdministracionTrait(relacionAlumnos + (asignatura -> List(alumno))))
      case l if l.contains(alumno) => None
      case l if l.size < asignatura.plazas => Some(new AdministracionTrait(relacionAlumnos + (asignatura -> (alumno :: l))))
      case l if l.size == asignatura.plazas && l.exists(_.repetidor) => Some(new AdministracionTrait(relacionAlumnos + (asignatura -> (alumno :: removeFirstElement(l, a=>a.repetidor)))))
      case _ => None
    }
  }

  def removeFirstElement(list: List[AlumnoTrait], f: AlumnoTrait =>Boolean): List[AlumnoTrait] = {

    @annotation.tailrec
    def go(acc: List[AlumnoTrait], rest: List[AlumnoTrait]): List[AlumnoTrait] = {
      rest match {
        case Nil => acc
        case h::t if f(h) => acc:::t //si cumple devuelvo la lista sin el
        case h::t if !f(h) => go(acc :+ h, t) //si no cumple paso el elemento al acumulado
      }
    }

    go(List(),list)
  }

}
