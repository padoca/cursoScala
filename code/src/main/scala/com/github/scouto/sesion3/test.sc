def composicion(f: Int => String, g: Int => Int): Int => String = {
  f compose g
}