package aoc
package day03

import scala.util.chaining.*
import aoc.board.Vec

def getValues(inp: List[String]): List[(Int, Set[Vec])] =
  for {
    (line, y) <- inp.zipWithIndex
    m         <- ("""(\d+)""".r).findAllMatchIn(line)
  } yield (m.matched.toInt, (m.start until m.end).map(x => Vec(x, y)).toSet)

def getNonValues(inp: List[String]): List[(String, Vec)] =
  for {
    (line, y) <- inp.zipWithIndex
    m         <- ("""([^0-9.])""".r).findAllMatchIn(line)
  } yield (m.matched -> Vec(m.start, y))

def surrounding(v: Vec): IndexedSeq[Vec] =
  for {
    x <- (-1 to 1)
    y <- (-1 to 1)
    if (x, y) != (0, 0)
  } yield (v + Vec(x, y))

def one(inp: String) =
  val scope = getNonValues(inp.split("\n").toList).flatMap((_, p) => surrounding(p)).toSet
  getValues(inp.split("\n").toList).filter((s, vs) => vs.exists(scope)).map(_._1).sum

def two(inp: String) =
  val values: List[(Int, Set[Vec])] = getValues(inp.split("\n").toList)
  val gears: List[Vec]              = getNonValues(inp.split("\n").toList).filter(v => v._1 == "*").map(_._2)
  val related: Vec => List[Int] = g => values.filter((v, ls) => surrounding(g).exists(ls)).map(_._1)

  gears
    .map(g => related(g).map(_.toLong))   // link surrounding values
    .filter(_.length == 2)                // should be related to exactly 2 values
    .map(inp => inp.head * inp.tail.head) // multiply
    .sum

object Solution extends App:

  one("day03.txt".live) pipe println
  two("day03.txt".live) pipe println
