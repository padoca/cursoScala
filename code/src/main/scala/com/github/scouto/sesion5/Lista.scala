package com.github.scouto.sesion5

/**
  * Created by scouto.
  */
sealed trait Lista[+A]
case object Vacio extends Lista[Nothing]
case class Cons[+A](head:A, tail:Lista[A]) extends Lista[A]


object Lista {


  def apply[A](as: A*): Lista[A] =
    if (as.isEmpty) Vacio
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints: Lista[Int]): Int = ???

  def product(ints: Lista[Double]) : Double = ???

  def tail[A](list: Lista[A]): Lista[A] = ???

  def setHead[A](list: Lista[A], newHead: A): Lista[A] = ???

  def drop[A](list: Lista[A],n: Int): Lista[A] = ???

  def dropWhile[A](list: Lista[A], f: A => Boolean): Lista[A] = ???

  def append[A](l1: Lista[A], l2: Lista[A]): Lista[A] = {

    l1 match {
      case Vacio => l2
      case Cons(h,t) => Cons (h, append(t, l2))
    }
  }


  def init[A](l: Lista[A]): Lista[A] = ???

  }



