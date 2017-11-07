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

  def sum(ints: Lista[Int]): Int = {
    ints match {
      case Vacio => 0
      case Cons (h, t) => h + sum(t)
    }
  }

  def product(ints: Lista[Double]) : Double = {
    ints match {
      case Vacio => 1.0
      case Cons (h, t) =>  h * product(t)
    }
  }

  def tail[A](list: Lista[A]): Lista[A] = {
    list match {
      case Vacio => Vacio
      case Cons (h, t) => t
    }
  }

  def setHead[A](list: Lista[A], newHead: A): Lista[A] = {
    list match {
      case Vacio => Lista(newHead)
      case Cons (h, t) => Cons(newHead,t)
    }
  }

  def drop[A](list: Lista[A],n: Int): Lista[A] = {
    @annotation.tailrec
    def goDrop(rest: Lista[A],n: Int): Lista[A] = {
      list match {
        case Vacio => Vacio
        case Cons (h, t) if (n == 0) => t
        case Cons (h, t) if (n > 0) => goDrop(t, n-1)
        case Cons (h, t) if (n < 0) => list
      }
    }

    list match {
      case Vacio => Vacio
      case Cons (h, t) => goDrop(list,n)
    }
  }

  def dropWhile[A](list: Lista[A], f: A => Boolean): Lista[A] = {
    @annotation.tailrec
    def goDropWhile(rest: Lista[A]): Lista[A] = {
      list match {
        case Vacio => Vacio
        case Cons (h, t) if f(h) => goDropWhile(t)
        case Cons (h, t) if !f(h)=> rest
      }
    }

    list match {
      case Vacio => Vacio
      case Cons (h, t) => goDropWhile(list)
    }

  }

  def append[A](l1: Lista[A], l2: Lista[A]): Lista[A] = {

    l1 match {
      case Vacio => l2
      case Cons(h,t) => Cons (h, append(t, l2))
    }
  }

 //recibida una lista, devuelva una Lista con todos los elementos excepto el último
  def init[A](l: Lista[A]): Lista[A] = {
    @annotation.tailrec
    def loop(acc: Lista[A], rest: Lista[A]): Lista[A] = {
      rest match {
        case Vacio => acc
        case Cons(h,t) =>  t match {
                              case Vacio => acc
                              case Cons(h,t) => loop(append(acc, Lista(h)),t)
                            }
      }

    }
    loop(Lista(), l)
  }

  }



