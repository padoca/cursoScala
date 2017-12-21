package com.github.scouto.sesion12

import scala.annotation.tailrec
import com.github.scouto.sesion12.Stream._
/**
  * Created by couto.
  */
sealed trait Stream[+A] {
  def headOption: Option[A] = {
    this match {
      case Empty => None
      case Cons(h, _) => Some(h())
    }
  }


  def toList: List[A] = {
    this match {
      case Empty => Nil
      case Cons(h,t) => h() :: t().toList
    }
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    this match {
      case Cons(h, t)  => f(h(), t().foldRight(z)(f))
      case _ => z
    }
  }

  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((elem,acc)=> cons(f(elem), acc ))
  }

  def take(n: Int): Stream[A] = {
    this match {
      //case Cons(h, t) if n > 1 => Cons(() => h(), () => t().take(n - 1))
      case Cons(h, t) if n > 1 => cons( h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => Stream(h())
      case _ => empty
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _ => empty
    }
  }

  //Sesion 12

  def mapUnfold[B](f: A => B): Stream[B] = {
    unfold(this) {
      case Cons(h,t)=> Some((f(h()), t()))
      case _ => None
    }
  }

  def takeUnfold(n: Int): Stream[A] = {
    unfold(this, n) {
      case (Cons(h,_),1)=> Some((h(), (empty, 0)))
      case (Cons(h,t),x) if x > 1 => Some((h(), (t(), x-1))) // x ha de ser positivo sino caso None
      case _ => None
    }
  }

  def takeWhileUnfold(p: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h,t) if p(h()) => Some((h(),t()))
      case _ => None
    }
  }

  def zipWith[B, C](other: Stream[B])(f: (A, B) => C): Stream[C] = {
    unfold(this, other) {
      case (Cons(h1,t1),Cons(h2,t2))=> Some((f(h1(),h2()), (t1(), t2())))
      case _ => None
    }
  }

  def zipWithAll[B, C](other: Stream[B])(f: (Option[A], Option[B]) => C) : Stream[C] = {
    unfold(this, other) {
      case (Cons(h1,t1),Cons(h2,t2))=> Some((f(Some(h1()),Some(h2())), (t1(), t2())))
      case (Cons(h1,t1), Empty) => Some((f(Some(h1()), None), (t1(), empty)))
      case (Empty, Cons(h2,t2)) => Some((f(None,Some(h2())), (empty,t2())))
      case _ => None
    }
  }

  def tails: Stream[Stream[A]] = {
    unfold(this) {
      case Cons(h,t)  => Some((Cons(h,t),t()))
      case _ => None
    }
  }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  //constructor
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl

    Cons(() => head, () => tail)
  }

  //constructor de empty Stream con tipo
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }

  def unfold[A,S](z: S)(f: S => Option[(A,S)]): Stream[A] = {
    f(z) match {
      case None =>empty
      case Some((a, s))=> cons(a, unfold(s)(f))
    }
  }

}


