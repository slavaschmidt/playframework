package play.routes.compiler

import java.io.File

import org.specs2.mutable.Specification

import scala.io.Source

object SwaggerFileParserSpec extends Specification {

  "swagger json file parser" should {

    "parse the basic swagger json" in {
      val (file: File, json: String) = readFile("petstore-simple.json")
      SwaggerJson.parse(json, file).isRight must beTrue
    }

    "parse the broken swagger json" in {
      val (file: File, json: String) = readFile("petstore-broke.json")
      SwaggerJson.parse(json, file).isRight must beFalse
    }

  }

  "swagger yaml file parser" should {

    "parse the basic swagger yaml" in {
      val (file: File, json: String) = readFile("petstore.yaml")
      SwaggerYaml.parse(json, file).isRight must beTrue
    }

    "parse the broken swagger json" in {
      val (file: File, json: String) = readFile("petstore-broke.yaml")
      SwaggerYaml.parse(json, file).isRight must beFalse
    }

  }

  def readFile(name: String): (File, String) = {
    val file = new File(getClass.getClassLoader.getResource(name).toURI)
    val json = Source.fromFile(file).getLines().mkString("\n")
    (file, json)
  }
}
