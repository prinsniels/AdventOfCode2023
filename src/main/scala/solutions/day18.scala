package aoc
package solutions
package day18

import scala.util.chaining.*
import aoc.utils.time.time

enum Dir(val x: Vec):
  case U extends Dir(Vec(0, -1))
  case R extends Dir(Vec(1, 0))
  case D extends Dir(Vec(0, 1))
  case L extends Dir(Vec(-1, 0))

case class Vec(x: Long, y: Long):
  def *(o: Long): Vec =
    Vec(x * o, y * o)

  def +(o: Vec): Vec =
    Vec(x + o.x, y + o.y)

case class Area(a: Vector[Vec])

object Area:
  def shoelace(a: Area): Long =
    val tot = a.a.zip(a.a.tail).foldLeft(0L) { case (acc, (l, r)) =>
      acc + l.x * r.y - l.y * r.x
    }
    math.abs(tot) / 2

  def pick(a: Long, intEdgePoints: Long): Long =
    math.abs(a) + intEdgePoints / 2 + 1

case class Action(d: Dir, steps: Int)

object Action:
  def parse(inp: String): Action =
    inp match
      case s"U $s ($c)" => Action(Dir.U, s.toInt)
      case s"D $s ($c)" => Action(Dir.D, s.toInt)
      case s"L $s ($c)" => Action(Dir.L, s.toInt)
      case s"R $s ($c)" => Action(Dir.R, s.toInt)

  def parse2(inp: String): Action =
    inp match
      case s"$d $s (#${c}3)" => Action(Dir.U, Integer.parseInt(c, 16))
      case s"$d $s (#${c}1)" => Action(Dir.D, Integer.parseInt(c, 16))
      case s"$d $s (#${c}2)" => Action(Dir.L, Integer.parseInt(c, 16))
      case s"$d $s (#${c}0)" => Action(Dir.R, Integer.parseInt(c, 16))

def dig(actions: Vector[Action], pos: Vec, acc: Vector[Vec]): Vector[Vec] =
  actions match
    case h +: t => dig(t, pos + (h.d.x * h.steps), acc :+ pos + (h.d.x * h.steps))
    case _      => acc

def part1(inp: String) =
  val actions = inp.split("\n").map(Action.parse).toVector
  val area    = Area(dig(actions, Vec(0, 0), Vector(Vec(0, 0))))

  Area.pick(Area.shoelace(area), actions.map(_.steps).sum)

def part2(inp: String) =
  val actions = inp.split("\n").map(Action.parse2).toVector
  val area    = Area(dig(actions, Vec(0, 0), Vector(Vec(0, 0))))

  Area.pick(Area.shoelace(area), actions.map(_.steps).sum)

object Solution extends App:
  time(part1("day18.txt".live)) pipe println
  time(part2("day18.txt".live)) pipe println
