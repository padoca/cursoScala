package com.github.scouto.sesion9

/**
  * Created by scouto.
  */
object Weekday extends Enumeration {

  //val Lunes, Martes, Miercoles, Jueves, Viernes, Sabado, Domingo = Value

//  case class WeekdayValue(val i: Int, val name: String, val laborable: Boolean, val horasQueTRabajo: Int) extends Val(i: Int, name: String) {
//      def isLaborable = laborable
//    }
//
//
//  val Monday = WeekdayValue(1, "Lunes", true, 8)

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




