import scala.io.Source

def readLines(resource: String): List[String] =
  Source
    .fromResource(resource)
    .getLines()
    .toList
