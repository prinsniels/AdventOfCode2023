package aoc
package solutions
package day13

import scala.util.chaining.*

case class Area(m: Vector[Vector[Char]])

object Area:
  def parse(inp: String): Area =
    Area(inp.split("\n").toVector.map(_.toVector))

def difference[T](l: Vector[T], r: Vector[T]): Int =
  l zip r count (_ != _)

def findPart(
    left: Vector[Vector[Char]],
    right: Vector[Vector[Char]],
    i: Int,
    maxDiff: Int = 0
): Option[Int] =
  if left.isEmpty then findPart(right.head +: left, right.tail, i + 1, maxDiff)
  else if right.isEmpty then None
  else if left.zip(right).map((l, r) => difference(l, r)).sum == maxDiff then Some(i)
  else findPart(right.head +: left, right.tail, i + 1, maxDiff)

def part1(inp: String) =
  inp
    .split("\n\n")
    .map(Area.parse)
    .map(a =>
      findPart(Vector.empty, a.m, 0).getOrElse(0) * 100 + findPart(Vector.empty, a.m.transpose, 0)
        .getOrElse(0)
    )
    .sum

def part2(inp: String) =
  inp
    .split("\n\n")
    .map(Area.parse)
    .map(a =>
      findPart(Vector.empty, a.m, 0, 1)
        .getOrElse(0) * 100 + findPart(Vector.empty, a.m.transpose, 0, 1)
        .getOrElse(0)
    )
    .sum

object Solution extends App:
  part1("day13.txt".live) pipe println
  part2("day13.txt".live) pipe println
