package fr.esgi.al.funprog

import fr.esgi.al.funprog.exceptions._
import play.api.libs.functional.syntax._
import play.api.libs.json._

final case class LawnMower(lawn: Lawn, start: Orientation, instructions: List[Char], current: Orientation) {
  def run() = current match {
    case _ if current == start => __run(instructions)
    case _                     => LawnMower(lawn, start, instructions).__run(instructions)
  }

  private def __run(instructions: List[Char]): LawnMower = instructions match {
    case Nil          => this
    case head :: tail => action(head).__run(tail)
  }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def action(action: Char): LawnMower = action match {
    case 'D' => right()
    case 'G' => left()
    case 'A' => forward()
    case _   => throw new DonneesIncorectesException("Invalid lawn mower action")
  }

  def right() = LawnMower(lawn, start, instructions, current.rotateRight())

  def left() = LawnMower(lawn, start, instructions, current.rotateLeft())

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def forward(): LawnMower = current.direction match {
    case 'N' => move(current.coordinates.x, math.min(current.coordinates.y + 1, lawn.limitY))
    case 'S' => move(current.coordinates.x, math.max(current.coordinates.y - 1, 0))
    case 'E' => move(math.min(current.coordinates.x + 1, lawn.limitX), current.coordinates.y)
    case 'W' => move(math.max(current.coordinates.x - 1, 0), current.coordinates.y)
    case _   => throw new DonneesIncorectesException("Invalid lawn mower direction")
  }

  private def move(x: Int, y: Int) = LawnMower(lawn, start, instructions, current.move(Coordinates(x, y)))
}

object LawnMower {
  def apply(lawn: Lawn, start: Orientation, instructions: List[Char]): LawnMower = LawnMower(lawn, start, instructions, start)
  def apply(lawn: Lawn, start: Orientation, instructions: String): LawnMower = LawnMower(lawn, start, instructions.toList, start)

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def load(lawn: Lawn, input: List[String]): LawnMower = input match {
    case line :: actions :: _ =>
      line.split(" ").toList match {
        case x :: y :: direction :: Nil =>
          try {
            val orientation = Orientation(Coordinates(x.toInt, y.toInt), direction(0))
            LawnMower(lawn, orientation, actions).run()
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
