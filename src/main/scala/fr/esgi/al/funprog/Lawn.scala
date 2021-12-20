package fr.esgi.al.funprog

import fr.esgi.al.funprog.exceptions._
import play.api.libs.json._

final case class Lawn(limitX: Int, limitY: Int)

object Lawn {
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def load(input: List[String]): Lawn = input match {
    case line :: Nil =>
      line.split(" ").toList match {
        case limitX :: limitY :: Nil =>
          try {
            Lawn(limitX.toInt, limitY.toInt)
          } catch {
            case _: NumberFormatException => throw new DonneesIncorectesException("Lawn size is not a number")
          }
        case _ => throw new DonneesIncorectesException("Invalid lawn input")
      }
    case Nil => throw new DonneesIncorectesException("Empty lawn input")
    case _   => throw new DonneesIncorectesException("Too many lawns")
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val writes: Writes[Lawn] = Writes { lawn =>
    Json.obj(
      "x" -> lawn.limitX,
      "y" -> lawn.limitY
    )
  }
}
