package aoc
package solutions
package day09

import scala.util.chaining.*
import scala.annotation.tailrec

def parse(inp: String): Vector[Long] =
  ("""([-\d]+)""".r).findAllIn(inp).map(_.toLong).toVector

def createNextLayer(base: Vector[Long]): Vector[Long] =
  base.sliding(2, 1).map(x => x(1) - x(0)).toVector

def extrapolateNext(pyramid: Vector[Vector[Long]]): List[Long] =
  pyramid.foldRight(List.empty[Long]) {
    case (layer, h :: t) => layer.last + h :: h :: t
    case (layer, _)      => List(0)
  }

def extrapolatePrev(pyramid: Vector[Vector[Long]]): List[Long] =
  pyramid.foldRight(List.empty[Long]) {
    case (layer, h :: t) => layer.head - h :: h :: t
    case (layer, _)      => List(0)
  }

def buildPyramid(layer: Vector[Long]): Vector[Vector[Long]] =
  @tailrec def helper(layer: Vector[Long], p: Vector[Vector[Long]]): Vector[Vector[Long]] =
    if layer.toSet == Set(0) then p.appended(layer)
    else helper(createNextLayer(layer), p.appended(layer))

  helper(layer, Vector.empty)

def part1(inp: String) =
  inp
    .split("\n")
    .map(parse)
    .map(buildPyramid)
    .map(extrapolateNext)
    .map(_.head)
    .sum

def part2(inp: String) =
  inp
    .split("\n")
    .map(parse)
    .map(buildPyramid)
    .map(extrapolatePrev)
    .map(_.head)
    .sum

object Solution extends App:
  part1("day09.txt".live) pipe println
  part2("day09.txt".live) pipe println
