package aoc
package solutions
package day11

import scala.util.chaining.*

def manhattanDist(l: Vec, r: Vec): Long =
  math.abs(l.x - r.x) + math.abs(l.y - r.y)

def ops[A](as: List[A]): List[(A, A)] =
  for {
    (a, ia) <- as.zipWithIndex
    (b, ib) <- as.zipWithIndex
    if ia < ib
  } yield (a, b)

case class Vec(x: Long, y: Long)
type Region = (Vec, Char)
type Galaxy = Vector[Region]

extension (s: String)
  def asGalaxy: Galaxy =
    val gs = for {
      (line, y) <- s.split("\n").zipWithIndex
      (c, x)    <- line.zipWithIndex
    } yield (Vec(x.toLong, y.toLong) -> c)
    gs.toVector

def stretchBy(
    g: Vector[Vector[Region]],
    m: Long,
    d: Long,
    acc: Galaxy,
    fdv: (Long, Vec) => Vec
): Galaxy =
  g match
    case hd +: t =>
      val nw = hd.map((k, v) => fdv(d, k) -> v)
      // do we need to stretch
      if (hd.map(_._2).toSet == Set('.')) then stretchBy(t, m, d + m, acc ++ nw, fdv)
      else stretchBy(t, m, d, acc ++ nw, fdv)
    case _ => acc

def stretchGalaxy(g: Galaxy, factor: Long): Galaxy =
  val xSliced = g.groupBy(_._1.x).toVector.sortBy(_._1).map(_._2)
  val x       = stretchBy(xSliced, factor, 0, Vector.empty, (d, v) => Vec(v.x + d, v.y))
  val ySliced = x.groupBy(_._1.y).toVector.sortBy(_._1).map(_._2)
  stretchBy(ySliced, factor, 0, Vector.empty, (d, v) => Vec(v.x, v.y + d))

def part1(inp: String) =
  val m         = 1
  val g         = inp.asGalaxy
  val stretched = stretchGalaxy(g, m)
  ops(stretched.filter((k, v) => v == '#').map(_._1).toList).map(manhattanDist).sum pipe println

def part2(inp: String) =
  val m         = 1000000 - 1
  val g         = inp.asGalaxy
  val stretched = stretchGalaxy(g, m)
  ops(stretched.filter((k, v) => v == '#').map(_._1).toList).map(manhattanDist).sum pipe println

object Solution extends App:
  part1("day11.txt".live)
  part2("day11.txt".live)
