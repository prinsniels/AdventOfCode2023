package aoc
package utils
package time

def time[T](block: => T): T = {
  val before = System.currentTimeMillis
  val result = block
  val after  = System.currentTimeMillis
  println("Elapsed time: " + (after - before) + "ms")
  result
}