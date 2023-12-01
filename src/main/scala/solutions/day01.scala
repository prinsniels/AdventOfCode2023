package aoc
package day01

import scala.util.chaining.*

def getDigits(line: String): Vector[String] =
  ("""\d""".r).findAllIn(line).toVector

def getDigetsExtended(line: String): Vector[String] =
  ("""(?=(one|two|three|four|five|six|seven|eight|nine|\d))""".r).findAllMatchIn(line).map(_.group(1)).toVector

def toDigitString(value: String): String =
  Map(
    "one"   -> "1",
    "two"   -> "2",
    "three" -> "3",
    "four"  -> "4",
    "five"  -> "5",
    "six"   -> "6",
    "seven" -> "7",
    "eight" -> "8",
    "nine"  -> "9"
  ).getOrElse(value, value)

object Solution extends App:
  lazy val one = "day01".live.map(getDigits).map(x => x.head + x.last).map(_.toInt).sum
  
  lazy val two = "day01".live
    .map(getDigetsExtended)
    .map(x => x.map(toDigitString))
    .map(x => x.head + x.last)
    .map(_.toInt)
    .sum
    
  one pipe println

  two pipe println