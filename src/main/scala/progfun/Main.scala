package fr.esgi.al.funprog

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import play.api.libs.functional.syntax._
import play.api.libs.json._

final case class DonneesIncorectesException(message: String) extends Exception(message)

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

@SuppressWarnings(Array("org.wartremover.warts.Throw"))
final case class Orientation(coordinates: Coordinates, direction: Char) {
  direction match {
    case 'N' | 'S' | 'E' | 'W' =>
    case _                     => throw new DonneesIncorectesException("Invalid lawn mower orientation")
  }

  def rotate(newDirection: Char): Orientation = Orientation(coordinates, newDirection)

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

final case class LawnMower(lawn: Lawn, start: Orientation, current: Orientation, instructions: List[Char]) {
  def run() = current match {
    case _ if current == start => __run(instructions)
    case _                     => LawnMower(lawn, start, start, instructions).__run(instructions)
  }

  private def __run(instructions: List[Char]): LawnMower = instructions match {
    case Nil          => this
    case head :: rest => action(head).__run(rest)
  }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def action(action: Char): LawnMower = action match {
    case 'D' => right()
    case 'G' => left()
    case 'A' => forward()
    case _   => throw new DonneesIncorectesException("Invalid lawn mower action")
  }

  def right(): LawnMower = current.direction match {
    case 'N' => rotate('E')
    case 'E' => rotate('S')
    case 'S' => rotate('W')
    case 'W' => rotate('N')
  }

  def left(): LawnMower = current.direction match {
    case 'N' => rotate('W')
    case 'W' => rotate('S')
    case 'S' => rotate('E')
    case 'E' => rotate('N')
  }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def forward(): LawnMower = current.direction match {
    case 'N' => move(current.coordinates.x, math.min(current.coordinates.y + 1, lawn.limitY))
    case 'S' => move(current.coordinates.x, math.max(current.coordinates.y - 1, 0))
    case 'E' => move(math.min(current.coordinates.x + 1, lawn.limitX), current.coordinates.y)
    case 'W' => move(math.max(current.coordinates.x - 1, 0), current.coordinates.y)
    case _   => throw new DonneesIncorectesException("Invalid lawn mower direction")
  }

  private def rotate(direction: Char) = LawnMower(lawn, start, current.rotate(direction), instructions)

  private def move(x: Int, y: Int) = LawnMower(lawn, start, current.move(Coordinates(x, y)), instructions)
}

object LawnMower {
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def load(lawn: Lawn, input: List[String]): LawnMower = input match {
    case line :: actions :: _ =>
      line.split(" ").toList match {
        case x :: y :: direction :: Nil =>
          try {
            val orientation = Orientation(Coordinates(x.toInt, y.toInt), direction(0))
            LawnMower(lawn, orientation, orientation, actions.toList).run()
          } catch {
            case _: NumberFormatException => throw new DonneesIncorectesException("Lawn mower position is not a number")
          }
        case _ => throw new DonneesIncorectesException("Invalid lawn mower input format")
      }
    case _ => throw new DonneesIncorectesException("Invalid lawn mower input")
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val writes: Writes[LawnMower] = Writes { lawnMower =>
    Json.obj(
      "debut"        -> lawnMower.start,
      "instructions" -> lawnMower.instructions.map(_.toString),
      "fin"          -> lawnMower.current
    )
  }
}

object Main extends App {
  val conf: Config = ConfigFactory.load()

  val inputFileName: String = conf.getString("application.input-file")
  val input = scala.io.Source.fromFile(inputFileName).getLines().toList
  val lawn = Lawn.load(input.take(1))

  val lawnMowers = input.drop(1).grouped(2).map(input => LawnMower.load(lawn, input)).toList

  println(
    Json.obj(
      "limite"    -> lawn,
      "tondeuses" -> lawnMowers
    )
  )
}
