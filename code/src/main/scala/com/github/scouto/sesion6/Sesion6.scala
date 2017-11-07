package com.github.scouto.sesion6

import com.github.scouto.sesion5.Lista.{append, sum}
import com.github.scouto.sesion5.{Cons, Lista, Vacio}

import scala.annotation.tailrec

object Sesion6 extends App {


  def foldRight[A,B](as: Lista[A], z: B) (f: (A, B) => B) : B = {
    as match {
      case Vacio => z
      case Cons (h, t) => f(h, foldRight(t,z)(f))
    }
  }

  def sumFold(ints: Lista[Int]): Int = {
    foldRight(ints, 0) ((a, b)=> a + b)
  }

  def productFold(ints: Lista[Double]) : Double = {
    foldRight(ints, 1.0) ((a, b)=> a * b)
    //foldRight(ints, 1.0) (_*_) //Idem a lo de arriba
  }

  def length[A](as: Lista[A]): Int = {
    foldRight(as, 0) ((elem, acc)=> acc+1)  //La funcion q le pasa es un elemento y el acumulado
  }

  @tailrec
  def foldLeft[A,B](as: Lista[A], z: B) (f: (B, A) => B) : B = {
    as match {
      case Vacio => z
      case Cons (h, t) => foldLeft(t,f(z,h))(f)
    }
  }

  def sumFoldLeft(ints: Lista[Int]): Int = {
    foldLeft(ints, 0) ((a, b)=> a + b)
    //foldLeft(ints, 1.0) (_+_)
  }

  def productFoldLeft(ints: Lista[Double]) : Double = {
    foldLeft(ints, 1.0) ((a, b)=> a * b)
  }

  def lengthFoldLeft[A](as: Lista[A]): Int = {
    foldLeft(as, 0) ((acc, elem)=> acc+1)
  }

  def reverse[A](as: Lista[A]): Lista [A] = {
    foldRight(as,Lista[A]()) ((elem, acc)=> Lista.append(acc,Lista(elem)))
    foldLeft(as,Lista[A]()) ((acc, elem)=> Lista.append(Lista(elem),acc))
    foldLeft(as,Lista[A]()) ((acc, elem)=> Cons(elem,acc))
  }

  def foldRightbyLeft[A,B](as: Lista[A], z: B) (f: (A, B) => B) : B = {
    foldLeft(as,z) ((a, b)=> f(b, a))
  }

  def foldLeftbyRight[A,B](as: Lista[A], z: B) (f: (B, A) => B) : B = {
    foldRight(as,z) ((elem, acc)=> f(acc, elem))
  }

  def productFoldRightLeft(ints: Lista[Double]) : Double = {
    foldRightbyLeft(ints, 1.0) ((a, b)=> a * b)
  }

  def productFoldLeftRight(ints: Lista[Double]) : Double = {
    foldLeftbyRight(ints, 1.0) ((a, b)=> a * b)
  }

  def lengthLeftRight[A](as: Lista[A]): Int = {
    foldLeftbyRight(as, 0) ((a, b)=> a+1)
  }

  def lengthRightLeft[A](as: Lista[A]): Int = {
    foldRightbyLeft(as, 0) ((a, b)=> b+1)
  }

  def appendFoldRight[A](a1: Lista[A], a2: Lista[A]): Lista[A] = {
    foldRight(a1,a2)((elem, acc)=> Cons(elem,acc))
  }

  def appendLists[A](as: Lista[Lista[A]]): Lista[A] = {
    //foldRight(as,Lista[A]()) ((elem, acc)=> appendFoldRight(elem,acc))
    foldRight(as,Lista[A]()) (appendFoldRight) //Currificaion de lo de arriba
  }
}
