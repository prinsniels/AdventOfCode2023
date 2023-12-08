package aoc

import scala.io.Source.fromFile
import java.io.File
import util.chaining.*

def readFile(path: String): String =
  val buffer = fromFile(new File(path))
  val lines = buffer.getLines().mkString("\n")
  buffer.close()
  lines


extension (a: String)
  def sample: String = readFile(s"src/main/resources/sample/$a")
  def live: String = readFile(s"src/main/resources/live/$a")