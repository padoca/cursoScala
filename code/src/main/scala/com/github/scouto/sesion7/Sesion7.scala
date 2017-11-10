package com.github.scouto.sesion7

import com.github.scouto.sesion5.{Cons, Lista, Vacio}
import com.github.scouto.sesion5.Lista._
import com.github.scouto.sesion6.Sesion6._
import com.github.scouto.sesion6.Sesion6.{foldRight, foldLeft, appendLists, _}
import scala.annotation.tailrec

object Sesion7 {
  //Nota: Se puede hacer con foldLeft pero no pasan los test por el orden en le resultado

  def addOne(l: Lista[Int]): Lista[Int] = {
    foldRight(l, Vacio:Lista[Int])((elem, acc)=>Cons(elem+1,acc));
  }

  def doubleToString(l: Lista[Double]): Lista[String] = {
    foldRight(l, Vacio:Lista[String])((elem, acc)=>Cons(elem.toString(),acc));
    //foldLeft(l, Vacio:Lista[String])((acc, elem)=>Cons(elem.toString(),acc));
  }

  def map[A, B](l: Lista[A])(f: A => B): Lista[B] = {
    foldRight(l, Vacio:Lista[B])((elem, acc)=>Cons(f(elem),acc));
    //foldLeft(l, Vacio:Lista[B])((acc, elem)=>Cons(f(elem),acc));
  }

  def filter[A](l: Lista[A])(f: A => Boolean): Lista[A] = {
    foldRight(l, Vacio:Lista[A])((elem, acc)=>if(!f(elem)) acc else Cons(elem,acc));
    //foldLeft(l, Vacio:Lista[A])((acc, elem)=>if(!f(elem)) acc else Cons(elem,acc));
  }

  def flatMap[A, B](l: Lista[A])(f: A => Lista[B]): Lista[B] = {
    appendLists(map(l)(f))
  }

  def filterFlatMap[A](l: Lista[A])(f: A => Boolean): Lista[A] = {
    flatMap(l)(elem=> if (f(elem)) Lista[A](elem) else Vacio:Lista[A] )
  }

  def addLists(l1: Lista[Int], l2: Lista[Int]): Lista[Int] = {
    l1 match {
      case Vacio => Vacio
      case Cons(h1,t1) => l2 match {
                          case Vacio => Vacio
                          case Cons(h2,t2) => Cons(h1+h2,addLists(t1,t2))
                        }
    }

    /*@tailrec
    def loop(acc: Lista[Int], rest1: Lista[Int], rest2: Lista[Int]): Lista[Int] = {
      (rest1, rest2) match {
        case (Vacio, Vacio) => acc
        case (Vacio, _) => Vacio
        case (_, Vacio) => Vacio
        case (Cons(h1, t1), Cons(h2, t2)) => loop(append(acc, Lista(h1+h2)), t1, t2)
      }


    }
    loop(Vacio, a1, a2)*/
  }

  def zipWith[A,B,C](l1: Lista[A], l2: Lista[B])(f:(A,B) => C): Lista[C] = {
    @tailrec
    def loop(acc: Lista[C], rest1: Lista[A], rest2: Lista[B]): Lista[C] = {
      (rest1, rest2) match {
        case (Vacio, Vacio) => acc
        case (Vacio, _) => Vacio
        case (_, Vacio) => Vacio
        case (Cons(h1, t1), Cons(h2, t2)) => loop(append(acc, Lista(f(h1, h2))), t1, t2)
      }
    }
      loop(Vacio, l1, l2)
  }

  /*def tieneSubsecuencia[A](lista: Lista[A], sub: Lista[A]): Boolean = ???*/
}
