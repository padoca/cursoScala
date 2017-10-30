package com.github.scouto.sesion2

/**
  * Created by scouto.
  */

import com.github.scouto.sesion2.Sesion2._
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.PropertyChecks

class Sesion2Test extends FlatSpec with Matchers with PropertyChecks{

  val genPositiveInteger = for (n <- Gen.choose(-500, 500)) yield n



  "sum" should "work with both natural numbers" in {
    sum(2,3) should be (5)
  }

  "sum" should "work for all numbers" in {

    forAll(genPositiveInteger, genPositiveInteger) { (n1: Int, n2: Int) =>
        val result = sum(n1, n2)
        println(s"${n1} + ${n2} equals: ${result}")
        result shouldEqual n1 + n2
    }
  }
}
