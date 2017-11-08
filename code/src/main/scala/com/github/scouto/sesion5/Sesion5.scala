package com.github.scouto.sesion5

import scala.annotation.tailrec

/**
  * Created by scouto.
  */
object Sesion5 extends App{




  def isSorted[A](as: List[A])(implicit ordered: (A,A) => Boolean): Boolean = {

    @tailrec
    def loop (rest: List[A]): Boolean = {
      rest match {
        case Nil => true
        case _::Nil => true
        case h1::t if ordered(h1, t.head) => loop (t)
        case h1::t if !ordered(h1, t.head) => false
      }
    }
    loop (as)
  }

}
