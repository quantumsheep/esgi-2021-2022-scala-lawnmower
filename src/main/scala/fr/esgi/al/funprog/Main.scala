package fr.esgi.al.funprog

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths

object Main extends App {
  val conf: Config = ConfigFactory.load()

  val inputFileName: String = conf.getString("application.input-file")
  val input = scala.io.Source.fromFile(inputFileName).getLines().toList

  val lawn = Lawn.load(input.take(1))
  val lawnMowers = input.drop(1).grouped(2).map(input => LawnMower.load(lawn, input)).toList

  val outputJsonFileName: String = conf.getString("application.output-json-file")
  val outputJsonPath = Files.write(Paths.get(outputJsonFileName), LawnMowerConverter.toJson(lawn, lawnMowers).toString.getBytes(StandardCharsets.UTF_8))
  println(s"JSON output written to ${outputJsonPath.toString()}")

  val outputCsvFileName: String = conf.getString("application.output-csv-file")
  val outputCsvPath = Files.write(Paths.get(outputCsvFileName), LawnMowerConverter.toCsv(lawnMowers).toString.getBytes(StandardCharsets.UTF_8))
  println(s"CSV output written to ${outputCsvPath.toString()}")

  val outputYamlFileName: String = conf.getString("application.output-yaml-file")
  val outputYamlPath = Files.write(Paths.get(outputYamlFileName), LawnMowerConverter.toYaml(lawn, lawnMowers).toString.getBytes(StandardCharsets.UTF_8))
  println(s"Yaml output written to ${outputYamlPath.toString()}")
}
