package play.routes.compiler

import java.io.File
import java.util.regex.Pattern

import com.fasterxml.jackson.core.{ JsonFactory, JsonParseException }
import com.fasterxml.jackson.databind.ObjectMapper

import scala.annotation.tailrec
import scala.collection.JavaConversions._

/**
 * @since   02.07.2015.
 */
private[routes] trait SwaggerRoutes {
  type SwaggerRoutesSpec = (String, File) => Either[Seq[RoutesCompilationError], String]
  def run(task: SpecificationTask): SwaggerRoutesSpec = task.run
}

private[routes] trait SpecificationTask {
  def jsonFactory: JsonFactory

  def run: SwaggerRoutes#SwaggerRoutesSpec = (json, file) => specify(json, file)

  type JMap = java.util.Map[String, Any]
  private val emptyJMap = new java.util.HashMap[String, Any]()

  def specify(input: String, source: File): Either[Seq[RoutesCompilationError], String] = try {
    val mapper = new ObjectMapper(jsonFactory)
    val spec = mapper.readValue(input, classOf[java.util.HashMap[String, Any]])
    val basePath = spec.getOrDefault("basePath", "/").toString
    val pathObjects = spec.getOrDefault("paths", emptyJMap).asInstanceOf[JMap].toMap
    val pathes = pathObjects filter { _._1.startsWith("/") } flatMap {
      case (path, definition) =>
        definition.asInstanceOf[JMap].toMap map {
          case (verb, singleSpec) =>
            val fullPath = basePath + replaceVars(path)
            val callDef = singleSpec.asInstanceOf[JMap].toMap.getOrElse("x-play-call-def",
              throw new IllegalArgumentException(s"Missing call definition (x-play-call-def) for path $fullPath")
            )
            verb.toUpperCase + "\t" + fullPath + "\t" + callDef
        }
    }
    Right(pathes.mkString("\n"))
  } catch {
    case e: JsonParseException =>
      Left(Seq(RoutesCompilationError(source, e.getMessage, Some(e.getLocation.getLineNr), Some(e.getLocation.getColumnNr))))
    case e: Exception =>
      Left(Seq(RoutesCompilationError(source, e.getMessage, None, None)))
  }

  private val paramRegexp = Pattern.compile("\\{([^}]+)\\}")

  @tailrec
  private def replaceVars(path: String): String =
    if (paramRegexp.matcher(path).find()) replaceVars(paramRegexp.matcher(path).replaceAll(":$1"))
    else path
}

case object ParseRoutesFromJson extends SpecificationTask {
  def jsonFactory = new JsonFactory()
}

case object ParseRoutesFromYaml extends SpecificationTask {
  def jsonFactory = new com.fasterxml.jackson.dataformat.yaml.YAMLFactory()
}

case object ParseNative extends SpecificationTask {
  override def run: SwaggerRoutes#SwaggerRoutesSpec = (input: String, file: File) => Right(input)
  override def jsonFactory: JsonFactory = null
}