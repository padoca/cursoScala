package com.github.scouto

/**
  * Created by scouto.
  */
object Sesion2 {

  def sum(x: Int, y: Int): Int = {
    x + y
  }

  def addToList(list: List[Int], elem: Int): List[Int] = list :+ elem

  def isPalindrome(list: List[Int]) : Boolean = list == list.reverse


  val romanos = Map(1 -> "I", 2->"II", 3->"III", 4->"IV", 5->"V", 6->"VI", 7->"VII", 8->"VIII", 9->"IX", 10→"X")

  def printMap(myMap: Map[Int, String]) = {
    myMap.foreach{
      case (k, v) => println(s"${k} => ${v}")
    }
  }

  def printSortedMap(myMap: Map[Int, String]) = {
    myMap.toList.sortBy(_._1).foreach{
      case (k, v) => println(s"${k} => ${v}")
    }
  }

  def aplicaInteres(cant: Double, tipo: Option[Double]): Double = {
    cant * tipo.getOrElse(1.5)
  }

  def aplicaInteres2(cant: Option[Double], tipo: Option[Double]): Option[Double] = {
    (cant, tipo) match {
      case (Some(c), Some(t)) => Some(c*t)
      case _ => None
    }
  }

  def aplicaInteresEither(cant: Either[String, Double], tipo: Either[String, Double]): Either[String, Double] =  {
    (cant, tipo) match {
      case (Left(e1), Left(e2)) => Left(s"errores encontrados: [${e1}, ${e2}]")
      case (Left(e), _) => Left(e)
      case (_, Left(e)) => Left(e)
      case (Right(c), Right(t)) => Right(c * t)
    }
  }

  def penultimate(list: List[Int]): Option[Int] = {
    list.reverse match {
      case h::Nil  => None
      case h::t => Some(t.head)
      case _ => None
    }
  }

  def duplicates(list: List[Int], k: Int): List[Int] = {
    //    def nthTimes(x: Int, n: Int) : List[Int] = {
    //      if (n==0) List()
    //      else x::nthTimes(x, n-1)
    //    }
    //
    //    list.flatMap(nthTimes(_, k))



    //  for {
    //    elem <- list
    //  } yield List.fill(k)(elem)
    //}.flatten


    //    list.flatMap(e => List.fill(k)(e))


    list.flatMap(x => (1 to k).map(_ => x))

  }

  def rotate(list: List[Int], x: Int): List[Int] = {
    list match {
      case Nil => list
      case l if (x > 0) => rotate(l.tail:::List(l.head),x-1)
      case l if (x < 0) => rotate(l.last::l.init,x-1)
    }

//    list match  {
//      case Nil => list
//      case h::Nil => list
//      case h::t if x == 0 => list
//      case h::t if x > 0 => rotate(t:+h, x-1)
//      case h::t if x < 0 => rotate(t.reverse.head::h::t.reverse.tail.reverse, x+1)
//    }
  }

  def isPalindrome(word: String): Boolean = {
    word.toUpperCase == word.reverse.toUpperCase
  }




}








