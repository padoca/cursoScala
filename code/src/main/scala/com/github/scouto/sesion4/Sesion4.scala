package com.github.scouto.sesion4

object Sesion3 extends App{
  //Quita el primer elemento que cumpla con una funcion pasada
  def removeFirstElement(list: List[Int], f: Int =>Boolean): List[Int] = {
     go(List(),list)

    @annotation.tailrec
    def go[A](acc: List[A], rest: List[A]): List[A] = {
      rest match {
        case Nil => acc
        case h::t if f(h) => acc:::t //si cumple devuelvo la lista sin el
        case h::t if !f(h) => go(acc :+ h, t) //si no cumple paso el elemento al acumulado
      }
    }
  }
}

