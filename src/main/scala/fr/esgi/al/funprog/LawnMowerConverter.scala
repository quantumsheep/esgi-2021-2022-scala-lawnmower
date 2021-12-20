package fr.esgi.al.funprog

import play.api.libs.json._

object LawnMowerConverter {
  def toJson(lawn: Lawn, lawnMowers: List[LawnMower]) = Json.obj(
    "limite"    -> lawn,
    "tondeuses" -> lawnMowers
  )

  def toCsv(lawnMowers: List[LawnMower]) =
    "numéro;début_x;début_y;début_direction;fin_x;fin_y;fin_direction;instructions" + toCsvLine(lawnMowers, 1)

  private def toCsvLine(lawnMowers: List[LawnMower], id: Int): String = lawnMowers match {
    case Nil => ""
    case head :: tail =>
      s"\n${id.toString};${head.start.coordinates.x.toString};${head.start.coordinates.y.toString};${head.start.direction.toString};${head.current.coordinates.x.toString};${head.current.coordinates.y.toString};${head.current.direction.toString};${head.instructions.mkString}" + toCsvLine(
        tail,
        id + 1
      )
  }

  def toYaml(lawn: Lawn, lawnMowers: List[LawnMower]) = fromJsObjectToYaml(toJson(lawn, lawnMowers))

  def fromJsObjectToYaml(json: JsObject) = __fromJsObjectToYaml(json, 0);

  private def __fromJsObjectToYaml(json: JsObject, indent: Int): String =
    json.fields.toList
      .map {
        case (key, value) => s"${__indent(indent)}$key: ${__fromJsValueToYaml(value, indent + 2, false)}"
        case _            => "???"
      }
      .mkString("\n")

  private def __fromJsObjectToYamlAfterArray(json: JsObject, indent: Int): String = json.fields.toList match {
    case Nil                 => ""
    case (key, value) :: Nil => s"$key: ${__fromJsValueToYaml(value, indent + 2, false)}"
    case (key, value) :: tail =>
      s"$key: ${__fromJsValueToYaml(value, indent + 2, false)}\n" + tail
        .map {
          case (key, value) => s"${__indent(indent)}$key: ${__fromJsValueToYaml(value, indent + 2, false)}"
          case _            => "???"
        }
        .mkString("\n")
  }

  private def __fromJsValueToYaml(value: JsValue, indent: Int, isArray: Boolean): String = value match {
    case JsString(value)  => s"${value}"
    case JsNumber(value)  => s"${value.toString}"
    case JsBoolean(value) => s"${value.toString}"
    case JsArray(value) =>
      s"\n${value
        .map { value =>
          s"${__indent(indent)}- ${__fromJsValueToYaml(value, indent + 2, true)}"
        }
        .mkString("\n")}"
    case value: JsObject =>
      isArray match {
        case true => __fromJsObjectToYamlAfterArray(value, indent)
        case _    => s"\n${__fromJsObjectToYaml(value, indent)}"
      }
    case _ => "???"
  }

  private def __indent(indent: Int) = "".padTo(indent, ' ').mkString("")
}
