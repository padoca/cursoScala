package com.github.scouto.sesion1

/**
  * Created by scouto.
  */

import com.github.scouto.sesion1.MyApp._
import org.scalatest._

class IntroTest extends FlatSpec with Matchers {



  "sum" should "work with both natural numbers" in {
    sum(2,3) should be (5)
  }

  "factorial" should "be 1 for any integer below 0" in {
    factorial(-5) should be (1)
    factorial(-1) should be (1)
  }

}
