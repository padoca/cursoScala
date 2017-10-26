package com.github.scouto.sesion3

import java.sql.{Date => SqlDate}
import java.util.{Date => UtilDate}


/**
  * Created by scouto.
  */
object Sesion3C extends App{

  val date = new SqlDate(System.currentTimeMillis())
  println(date.getClass)

  val dateU = new UtilDate(System.currentTimeMillis())
  println(dateU.getClass)




  def uncurry(f: Int => Int => Int): (Int, Int) => Int = {
    (a, b) => f(a)(b)
  }

  def curry(f: (Int, Int) => Int): Int => Int => Int = {
    a => b =>f(a,b)
  }

  def composicion(f: Int => String, g: Int => Int): Int => String = {
    a => f(g(a))
  }

  def composicion2(f: Int => String, g: Int => Int) = {
    a: Int => f(g(a))
  }

  def genericuncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  def genericcurry[A,B,C](f: (A, B) => C): A => B => C = {
    a => b =>f(a,b)
  }

  def genericComposicion[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }


}

class PersonC (private val _name: String, private var _age: Int = 0) {

 def name = _name

 def age = _age

  def age_(newAge: Int) = _age = newAge

}

