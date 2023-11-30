package aoc

import scala.io.Source.fromFile
import java.io.File
import util.chaining.*

def readFile(path: String): List[String] =
  val buffer = fromFile(new File(path))
  val lines = buffer.getLines().toList
  buffer.close()
  lines

extension (a: String)
  def sample: List[String] = readFile(s"src/main/resources/sample/$a")
  def live: List[String] = readFile(s"src/main/resources/live/$a")
