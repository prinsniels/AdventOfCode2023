package aoc
package solutions
package day16

import scala.util.chaining.*

import scala.annotation.tailrec
import aoc.utils.time.time

enum Dir(val x: Int, val y: Int):
  case N extends Dir(0, -1)
  case E extends Dir(1, 0)
  case S extends Dir(0, 1)
  case W extends Dir(-1, 0)

case class Beam(d: Dir, p: Vec):
  def step(d: Dir): Beam =
    Beam(d, p + d)

enum Tile:
  case MU, MD, SU, SF, DOT

object Tile:
  def parse(c: Char): Tile =
    c match
      case '/'  => MU
      case '\\' => MD
      case '|'  => SU
      case '-'  => SF
      case '.'  => DOT

case class Vec(x: Int, y: Int):
  def +(d: Dir) = Vec(x + d.x, y + d.y)

type Area[T] = Vector[Vector[T]]

object Area:
  def parse(inp: String) =
    inp.split("\n").toVector.map(_.map(Tile.parse).toVector)

extension [T](a: Area[T])
  def mX = a.size - 1
  def mY = a.size - 1

extension (a: Area[Tile])
  def asString: Area[String] = a.map(_.map {
    case Tile.MU  => "/"
    case Tile.MD  => "\\"
    case Tile.SU  => "|"
    case Tile.SF  => "-"
    case Tile.DOT => "."
  })

extension (a: Area[String])
  def show: Unit =
    a.foreach(_.mkString pipe println)

type Graph = Beam => List[Beam]

def mkGraph(a: Area[Tile]): Graph =
  b =>
    val options = (a(b.p.y)(b.p.x), b.d) match
      case (Tile.MU, Dir.N)         => b.step(Dir.E) :: Nil
      case (Tile.MU, Dir.E)         => b.step(Dir.N) :: Nil
      case (Tile.MU, Dir.S)         => b.step(Dir.W) :: Nil
      case (Tile.MU, Dir.W)         => b.step(Dir.S) :: Nil
      case (Tile.MD, Dir.N)         => b.step(Dir.W) :: Nil
      case (Tile.MD, Dir.E)         => b.step(Dir.S) :: Nil
      case (Tile.MD, Dir.S)         => b.step(Dir.E) :: Nil
      case (Tile.MD, Dir.W)         => b.step(Dir.N) :: Nil
      case (Tile.SU, Dir.E | Dir.W) => b.step(Dir.N) :: b.step(Dir.S) :: Nil
      case (Tile.SF, Dir.N | Dir.S) => b.step(Dir.E) :: b.step(Dir.W) :: Nil
      case (_, d)                   => b.step(d) :: Nil
    options.filterNot(d => d.p.x < 0 || d.p.y < 0 || d.p.y > a.mY || d.p.x > a.mX)

@tailrec
def energize(g: Graph, act: List[Beam], vis: Set[Beam]): Set[Beam] =
  if act.isEmpty then vis
  else
    val nw = act.flatMap(g).filterNot(vis)
    energize(g, nw, vis ++ nw)

def part1(inp: String) =
  val area  = Area.parse(inp)
  val graph = mkGraph(area)
  val start = Beam(Dir.E, Vec(0, 0))
  energize(graph, List(start), Set(start)).map(b => b.p).size

extension (d: Dir)
  def oppo = d match
    case Dir.N => Dir.S
    case Dir.E => Dir.W
    case Dir.S => Dir.N
    case Dir.W => Dir.E

def part2(inp: String) =
  val area  = Area.parse(inp)
  val graph = mkGraph(area)

  val xs = (0 to area.mX).flatMap(x => List(Beam(Dir.S, Vec(x, 0)), Beam(Dir.N, Vec(x, area.mY))))
  val ys = (0 to area.mY).flatMap(y => List(Beam(Dir.E, Vec(0, y)), Beam(Dir.W, Vec(area.mX, y))))

  (xs ++ ys).foldLeft(0) { case (c, start) =>
    val s = energize(graph, List(start), Set(start)).map(b => b.p).size
    if s > c then s else c
  }


object Solutions extends App:
  time(part1("day16.txt".live)) pipe println
  time(part2("day16.txt".live)) pipe println
