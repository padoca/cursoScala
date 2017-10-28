package com.github.scouto.sesion4

object Sesion4 extends App{
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

  def fb(x: Int): BigInt = {
    if (x<0) throw new ArithmeticException("debe ser mayo dre 0")
    x match {
      case 0 => 0
      case 1 => 1
      case _ => fb(x-1) + fb(x-2)
    }
  }

  //def msort[T](l: List[T], less(T, T) => Boolean):List[T] = {  }
  //def isSorted[A](as: List[A])(implicit ordered: (A,A) => Boolean): Boolean = {  }
}

