package com.github.scouto

/**
  * Created by scouto.
  */
object MyApp extends App {
  println("Hello World ")

  def sum(x: Int, y: Int): Int = {
    x + y
  }






  def wrongFactorial(n: Int): Int = {
    if (n <= 0) 1
    else n * factorial(n - 1)
  }










  def factorial(n: Int): Int = {

    @annotation.tailrec
    def rec(acc: Int, current: Int): Int = {
      if (current <= 0) acc
      else rec (acc*current, current -1)

    }

    rec (1, n)
  }


  def operate[A,B] (x: A, y:A)(f: (A, A) => B) = f(x,y)

  def max(list: List[Int]): Int = ???

  def second(list: List[Int]) : Int = ???

  def nth(list: List[Int], n: Int): Int = ???





  }








