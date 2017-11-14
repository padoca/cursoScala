package com.github.scouto.sesion8

/**
  * Created by couto
  */
sealed trait Option[+A] {

  def map[B] (f: A => B): Option[B] = {
    this match {
      case None => None
      case Some(a) => Some(f(a))
    }
  }

  def flatMap[B](f: A => Option[B]) : Option[B] = {
    map(f) getOrElse None
  }

  def getOrElse[B >: A] (default: => B): B = {
    this match {
      case None => default
      case Some(a) => a
    }
  }

  //devuelve el valor del Option si existe, en caso contrario devuelve el parámetro recibido
  def orElse[B >: A] (ob: => Option[B]): Option[B] = {
    map(Some(_)) getOrElse ob
  }

  def filter(f: A => Boolean): Option[A] = {
    flatMap(a => if (f(a)) Some(a) else None)
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]



object Option {


  def mean(xs:Seq[Double]): Option[Double] = {
    if(xs.isEmpty) None
    else Some (xs.sum / xs.length)

    //else Some(xs.foldLeft(0.0)(_+_) / xs.length)
  }

  def calcularCuota(age: Int, incidencias: Int): Double = {
    age * incidencias
  }

  def calcularCuotaString(age: String, incidencias: String): Option[Double] = {
    var myAge = try {
      age.toInt
    } catch {
      case _: Exception => return None
    }
    var myIncidencias = try {
      incidencias.toInt
    } catch {
      case _: Exception => return None
    }
    Some(calcularCuota(myAge,myIncidencias))
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    (a,b) match {
      case (Some(l), Some(r) ) => Some(f(l,r))
      //case (_,_) => None
      case _ => None
    }
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = ???

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = ???

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = ???

  def variance(xs: Seq[Double]): Option[Double] = ???

}