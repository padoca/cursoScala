package com.github.scouto.sesion9

import scala.util.{Failure, Success, Try}

/**
  * Created by couto on 30/06/17.
  */
sealed trait Either[+E, +A] {



  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Left(a) => Left(a)
      case Right(a) => Right(f(a))
    }
  }

  def flatMap [EE >: E, B >: A](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(a) => Left(a)
      case Right(a) => f(a)
    }
  }

  def orElse [EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(a) => b
      case Right(a) => Right(a)
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C) : Either[EE, C] = {
    (this, b) match {
      case (Right(a),Left(b)) => Left(b)
      case (Left(a), Left(b)) => Left(a)//Cualquier error vale, es mas, nos podemos ahorrar este case con el _ en los otros
      case (Left(a), Right(b)) => Left(a)
      case (Right(a), Right(b)) => Right(f(a,b))
    }
  }

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]


object Either {


  def mean(xs:Seq[Double]): Either[String, Double] = {
    if(xs.isEmpty) Left("Empty Seq")
    else Right(xs.sum / xs.length)
  }

  def calcularCuota(age: Int, incidencias: Int): Double = {
    age * incidencias
  }

  def calcularCuotaString(age: String, incidencias: String): Either[String, Double] = {

    def convertToEither(x:String): Either[String, Int] = {
      Try{x.toInt} match {
        case Failure(a) => Left(a.getMessage)
        case Success(a) => Right(a)
      }
    }

    convertToEither(age).map2(convertToEither(incidencias))(calcularCuota) // (a,b)=>calcularCuota(a,b)
  }


  def sequence[E, A](a: List[Either[E, A]]): Either[E, List[A]] = {
    a.foldRight(Right(Nil): Either[E, List[A]])((elem,acc)=>elem.map2(acc)(_::_))
  }

  def traverse[E, A, B](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    a.foldRight(Right(Nil): Either[E, List[B]])((elem,acc)=>f(elem).map2(acc)(_::_))
  }

  def sequenceViaTraverse[E, A](a: List[Either[E, A]]): Either[E, List[A]] = {
    traverse(a)(a=>a)
  }
}
