import com.github.scouto.sesion4._
import scala.util._

val pClass = new Persona("Paco", "Garcia", 45)

val pObject = Persona("Pedro", "Garcia", 45)

pObject match {
  case Persona("Pavo", "Garcia", 45) => "Paco"
  case Persona(a, _, 45) => a
  case _ => "Nadie"
}


val q = "Texto"
Try(q.toInt).isSuccess
Try(q.toInt).getOrElse("Excepcion")
Try(q.toInt).get

def removeFirstElement(list: List[Int], f: Int =>  Boolean): List[Int] = {

}