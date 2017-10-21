package com.github.scouto

/**
  * Created by scouto.
  */
object Sesion2 {

  def sum(x: Int, y: Int): Int = {
    x + y
  }

  def addToList(list: List[Int], elem: Int): List[Int] = ???

  def isPalindrome(list: List[Int]) : Boolean = ???


  val romanos = Map(1 → "I", 2->"II", 3->"III", 4->"IV", 5->"V", 6->"VI", 7->"VII", 8->"VIII", 9->"IX", 10→"X")


  def aplicaInteres(cant: Double, tipo: Option[Double]): Double = ???

  def aplicaInteres2(cant: Option[Double], tipo: Option[Double]): Option[Double] = ???

  def aplicaInteresEither(cant: Either[String, Double], tipo: Either[String, Double]): Either[String, Double] =  ???

  def penultimate(list: List[Int]): Option[Int] = ???

  def duplicates(list: List[Int], k: Int): List[Int] = ???

  def rotate(list: List[Int], x: Int): List[Int] = ???

  def isPalindrome(word: String): Boolean = ???




}








