package com.github.scouto.sesion3

import java.sql.{Date => SqlDate}
import java.util.{Date => UtilDate}


/**
  * Created by scouto.
  */
object Sesion3 extends App{

  val date = new SqlDate(System.currentTimeMillis())
  println(date.getClass)

  val dateU = new UtilDate(System.currentTimeMillis())
  println(date.getClass)

 //Separar funcion en dos parametros. Ojo:Devuelvo una funcion
  def uncurry(f: Int => Int => Int): (Int, Int) => Int = {
    (a, b) => f(a)(b)
  }


  def curry(f: (Int, Int) => Int): Int => Int => Int = {
    a => b => f(a,b)
  }

  //Una funcion que se aplica sobre el resultado de otra funcion
  def composicion(f: Int => String, g: Int => Int): Int  => String = {
    //Forma facil con compose (compose concatena funciones):
    //f compose g
    a => f(g(a))
  }
  //Misma funcion donde no le dices lo q devuelve
  def composicion2(f: Int => String, g: Int => Int)  = {
    a: Int => f(g(a))
  }


  //Forma generica (A, B y C son tipos genericos)
  def uncurryGen[A,B,C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  def curryGen[A,B,C](f: (A, B) => C): A => B => C = {
    a => b => f(a,b)
  }
  //Pasa de A a C pasando por B(EN el original de Int a String pasando por Int)
  def composicionGen[A,B,C](f: B => C, g: A => B): A  => C = {
    a => f(g(a))
  }

}

class Person (private val _name: String, private var _age: Int = 0) {

 def name = _name

 def age = _age

 def age_(newAge: Int) = _age = newAge

}


