package play.routes.compiler

import java.io.File

trait RoutesParser {
  /**
   * Parse the given input file to a list of route rules
   *
   * @param inputFile The routes file to parse
   * @return Either the list of compilation errors encountered, or a list of routing rules
   */
  def parse(inputFile: File): Either[Seq[RoutesCompilationError], List[Rule]]
}
