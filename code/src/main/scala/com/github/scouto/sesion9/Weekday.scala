package com.github.scouto.sesion9

/**
  * Created by scouto.
  */
object Weekday extends Enumeration {

  //val Lunes, Martes, Miercoles, Jueves, Viernes, Sabado, Domingo = Value

  val Monday = Value("Lunes")
  val Tuesday = Value("Martes")
  val Wednesday = Value("Miercoles")
  val Thursday = Value("Jueves")
  val Friday = Value("Viernes")
  val Saturday = Value("Sabado")
  val Sunday = Value("Domingo")

}

object MyApp extends App{

  def laborable(weekday: Weekday.Value): Boolean = {
    weekday match {
      case Weekday.Monday => false
      case Weekday.Tuesday => false
      case Weekday.Wednesday => false
      case Weekday.Thursday  => false
      case Weekday.Friday => false
      case Weekday.Saturday => true
      case Weekday.Sunday => true
    }
//    weekday.id match {
//      case a => if(a<6)false else true
//    }
  }

  //println(Weekday.values.toList.sorted)//Ordenado por id
  println(Weekday.values.toList.sortBy(a=>a.toString))//Ordenado por nombre

}




