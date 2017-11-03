package com.github.scouto.sesion4

object Sesion4 extends App{
  //Quita el primer elemento que cumpla con una funcion pasada
  def removeFirstElement(list: List[Int], f: Int =>Boolean): List[Int] = {
     go(List(),list)

    @annotation.tailrec
    def go(acc: List[Int], rest: List[Int]): List[Int] = {
      rest match {
        case Nil => acc
        case h::t if f(h) => acc:::t //si cumple devuelvo la lista sin el
        case h::t if !f(h) => go(acc :+ h, t) //si no cumple paso el elemento al acumulado
      }
    }
    /*def go[A](acc: List[A], rest: List[A]): List[A] = {
      rest match {
        case Nil => acc
        case h::t if f(h) => acc:::t //si cumple devuelvo la lista sin el
        case h::t if !f(h) => go(acc :+ h, t) //si no cumple paso el elemento al acumulado
      }
    }*/
  }

  def fb(x: Int): BigInt = {
    if (x<0) throw new ArithmeticException("debe ser mayo dre 0")
    x match {
      case 0 => 0
      case 1 => 1
      case _ => fb(x-1) + fb(x-2)
    }
  }

  def msort[T](l: List[T], less:(T, T) => Boolean):List[T] = {
    @annotation.tailrec
    def accsort (acc: List[T], pending: List[T]): List[T] ={
      pending match {
        case h::Nil => acc
        case h::t if less(h, t.head) && !isSorted(t.tail) => accsort(acc:::List(h):::List(t.head), t.tail)
        case h::t if less(h, t.head) && isSorted(t.tail) => acc:::pending
        case h::t if !less(h, t.head) => accsort(acc:::List(t.head):::List(h), t.tail)
        case _ => acc

      }

    }
    accsort(List(), l)

  }

  def isSorted[A](as: List[A])(implicit ordered: (A,A) => Boolean): Boolean = {

    @annotation.tailrec
    def loop (rest: List[A]): Boolean = {
      rest match {
        case Nil => true
        case h::Nil => true
        case h1::t if ordered(h1, t.head) => loop (t)
        case h1::t if !ordered(h1, t.head) => false
      }
    }
    loop (as)
  }

}

