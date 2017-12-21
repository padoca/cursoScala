package com.github.scouto.sesion13

/**
  * Created by couto on 5/07/17.
  */
import scala.annotation.tailrec


trait RNG {
  def nextInt: (Int, RNG)
}


object RNG {

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = SimpleRNG(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }


  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    rng.nextInt match {
      case (Integer.MIN_VALUE, newRNG) => (0, newRNG)
      case (x, newRNG) if x < 0=> (Math.abs(x), newRNG)
      case (x, newRNG) => (x, newRNG)
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Integer.MAX_VALUE.toDouble + 1), r)
  }


  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i,d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r1) = intDouble(rng)
    ((d, i), r1)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1,d2,d3), r3)
  }

  def ints(n: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def loop(acc: List[Int], currentCount: Int)(currentRng: RNG): (List[Int], RNG) = {

      currentCount match {
        case x if x <= 0 => (acc, currentRng)
        case x if x > 0 => {
          val (i, newRNG) = currentRng.nextInt
          loop(i::acc, x-1)(newRNG)
        }
      }
    }
    loop(Nil, n)(rng)
  }


  type Rand[+A] = RNG => (A, RNG)
//    type Rand[+A] = State[RNG, A]

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s:Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }

  def nonNegativeEven: Rand[Int] = {
    map(nonNegativeInt)(x => x - x % 2)
  }

  def doubleMap: Rand[Double] = {
    map(nonNegativeInt)(x => x / (Integer.MAX_VALUE.toDouble + 1))
  }

  def map2[A,B, C](ra:Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }

  def  both[A, B](ra:Rand[A], rb: Rand[B]): Rand[(A,B)] = {
    //    map2(ra, rb)((a,b) => (a,b))
    map2(ra, rb)((_,_))
  }

  def intDoubleBoth: Rand[(Int, Double)] = {
    both(int, double)
  }

  def doubleIntBoth: Rand[(Double, Int)] = {
    both(double, int)
  }


  //Sesion 13
  def sequence[A](l: List[Rand[A]]): Rand[List[A]] = {
    l.foldRight(unit[List[A]](Nil))((elem, acc) => map2(elem, acc)(_ :: _))
  }

  def intsBySeq(n: Int): Rand[List[Int]] = {
    sequence(List.fill(n)(int))
  }


  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng)
    }
  }

  //Primera version => Que pasa si MaxInt no es divisiblepor max? Numeros menores aparecerán mas veces
  //Cuando nonNegativeInt genere numeros mas grandes que el multiplo mas grande de max hay que repetirla llamada
  def nonNegativeLessThan1(max: Int): Rand[Int] = {
    map(nonNegativeInt)(_ % max)
  }

  //  //Segunda version => Necesitamos un RNG, debemos pasarselo explicito en lugar de usando map
  //  def nonNegativeLessThan2(max: Int): Rand[Int] = {
  //    map(nonNegativeInt){i =>
  //      val mod = i % max
  //      if (i + (max -1) -mod >= 0) mod else nonNegativeLessThan2(max)
  //    }
  //  }

  //Tercera version => OK Pero es un poco peñazo
  def nonNegativeLessThan2(max: Int): Rand[Int] = {
    rng => {
      val (i, rng2) = nonNegativeInt(rng)
      val mod = i % max
      if (i + (max - 1) - mod >= 0) (mod, rng2) else nonNegativeLessThan2(max)(rng2)
    }
  }


  //Cuarta version => La buena
  def nonNegativeLessThan(max: Int): Rand[Int] = {
    flatMap(nonNegativeInt) {
      i =>
        val mod = i % max
        if (i + (max - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(max)
    }
  }

  def mapFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => unit(f(a)))
  }

  def map2FlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => map(rb)(b => f(a, b)))
  }


  def generalMap[S, A, B](s: S => (A, S))(f: A => B): S => (B, S) = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }

  //  type State[S, +A] = S => (A,S)

}

  import State._


  case class State[S, +A](run: S => (A,S)) {

    def map[B](f: A => B): State[S, B] = {
      flatMap(a => unit(f(a)))
//      State(s => {
//        val (a, rng2) = run(s)
//        (f(a), rng2)
//      })
    }


    def map2[B, C](s2: State[S, B])(f: (A, B) => C): State[S, C] = {
      flatMap(a => s2.map(b => f(a,b)))
//      State(s => {
//        val (a, s2) = run(s)
//        val (b, s3) = run(s2)
//        (f(a, b), s3)
//      })
    }

    def flatMap[B](f: A => State[S, B]): State[S, B] = {
      State(s => {
        val (a, s1) = run(s)
        f(a).run(s1)
      })
    }
  }

  object State {
    def unit[S, A](a: A): State[S, A] = State(s => (a, s))

    def sequence[S, A](l: List[State[S, A]]): State[S, List[A]] = {
      l.foldRight(unit[S, List[A]](Nil))((elem, acc) => elem.map2(acc)(_ :: _))
    }

    type newRand[+A] = State[RNG, A]

    val int: newRand[Int] = State(s => s.nextInt)

    def ints(n: Int): newRand[List[Int]] = {
      sequence(List.fill(n)(int))
    }


  }












