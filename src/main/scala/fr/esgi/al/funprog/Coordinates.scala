package fr.esgi.al.funprog

import play.api.libs.json._

final case class Coordinates(x: Int, y: Int)

object Coordinates {
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val writes: Writes[Coordinates] = Writes { coordinates =>
    Json.obj(
      "x" -> coordinates.x,
      "y" -> coordinates.y
    )
  }
}
