package fr.esgi.al.funprog

import fr.esgi.al.funprog.exceptions._
import play.api.libs.json._

@SuppressWarnings(Array("org.wartremover.warts.Throw"))
final case class Orientation(coordinates: Coordinates, direction: Char) {
  direction match {
    case 'N' | 'S' | 'E' | 'W' =>
    case _                     => throw new DonneesIncorectesException("Invalid lawn mower orientation")
  }

  def rotateRight(): Orientation = direction match {
    case 'N' => Orientation(coordinates, 'E')
    case 'E' => Orientation(coordinates, 'S')
    case 'S' => Orientation(coordinates, 'W')
    case 'W' => Orientation(coordinates, 'N')
  }

  def rotateLeft(): Orientation = direction match {
    case 'N' => Orientation(coordinates, 'W')
    case 'W' => Orientation(coordinates, 'S')
    case 'S' => Orientation(coordinates, 'E')
    case 'E' => Orientation(coordinates, 'N')
  }

  def move(newCoordinates: Coordinates): Orientation = Orientation(newCoordinates, direction)
}

object Orientation {
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val writes: Writes[Orientation] = Writes { orientation =>
    Json.obj(
      "point"     -> orientation.coordinates,
      "direction" -> orientation.direction.toString
    )
  }
}
