package com.github.scouto.sesion9

object WeekdayLab extends Enumeration {

  case class WeekdayLabVal(val i: Int, val name: String, val laborable: Boolean) extends Val(i: Int, name: String) {
          def isLaborable = laborable
  }


  val Monday = WeekdayLabVal(0, "Lunes", true)
  val Tuesday = WeekdayLabVal(1, "Martes", true)
  val Wednesday = WeekdayLabVal(2, "Miercoles", true)
  val Thursday = WeekdayLabVal(3, "Jueves", true)
  val Friday = WeekdayLabVal(4, "Viernes", true)
  val Saturday = WeekdayLabVal(5, "Sabado", false)
  val Sunday = WeekdayLabVal(6, "Domingo", false)


}

object WeekdayLabApp extends App {

  def isLaborable(weekday: WeekdayLab.WeekdayLabVal): Boolean = {
    weekday.isLaborable
  }

  println(WeekdayLab.values.toList.sortBy(_.toString))
}