package aoc
package solutions
package day14

import scala.util.chaining.*
import aoc.utils.time.time
import scala.annotation.tailrec
import scala.collection.immutable.HashMap

sealed trait Bolder:
  val x: Int
  val y: Int

case class H(x: Int, y: Int) extends Bolder
case class O(x: Int, y: Int) extends Bolder

object Bolder:
  def parse(c: Char, x: Int, y: Int): Bolder =
    c match
      case 'O' => O(x, y)
      case '#' => H(x, y)

case class Platform(bs: Set[Bolder], mX: Int, mY: Int)

extension (p: Platform)
  def score: Int =
    p.bs.toVector.filter(_.isInstanceOf[O]).map(b => p.mY + 1 - b.y).sum

  def show =
    val m = p.bs.map(b => (b.x, b.y) -> b).toMap
    (0 to p.mY).foreach { y =>
      (0 to p.mX)
        .map(x => {
          m.get((x, y))
            .map(b =>
              b match
                case H(x, y) => "#"
                case O(x, y) => "O"
            )
            .getOrElse(".")
        })
        .mkString pipe println
    }

object Platform:
  def fromString(inp: String): Platform =
    val bs = for {
      (line, y) <- inp.split("\n").zipWithIndex
      (c, x)    <- line.zipWithIndex
      if c != '.'
    } yield (Bolder.parse(c, x, y))

    Platform(bs.toSet, bs.map(_.x).max, bs.map(_.y).max)

def tiltNorth(p: Platform): Platform =
  p.copy(bs =
    p.bs
      .groupBy(_.x)
      .map((x, bs) => x -> bs.toVector.sortBy(_.y))
      .flatMap((x, bs) => shift(bs, O(x, 0), b => O(b.x, b.y + 1)))
      .toSet
  )

def tiltEast(p: Platform): Platform =
  p.copy(bs =
    p.bs
      .groupBy(_.y)
      .map((y, bs) => y -> bs.toVector.sortBy(-_.x))
      .flatMap((y, bs) => shift(bs, O(p.mX, y), b => O(b.x - 1, b.y)))
      .toSet
  )

def tiltSouth(p: Platform): Platform =
  p.copy(bs =
    p.bs
      .groupBy(_.x)
      .map((x, bs) => x -> bs.toVector.sortBy(-_.y))
      .flatMap((x, bs) => shift(bs, O(x, p.mY), b => O(b.x, b.y - 1)))
      .toSet
  )

def tiltWest(p: Platform): Platform =
  p.copy(bs =
    p.bs
      .groupBy(_.y)
      .map((y, bs) => y -> bs.toVector.sortBy(_.x))
      .flatMap((y, bs) => shift(bs, O(0, y), b => O(b.x + 1, b.y)))
      .toSet
  )

def shift(v: Vector[Bolder], bound: O, lean: Bolder => O): Vector[Bolder] =
  v.foldLeft(Vector.empty[Bolder], Option.empty[Bolder]) {
    case ((acc, _), b @ H(x, y))          => acc.appended(b)          -> Some(b)
    case ((acc, Some(prev)), b @ O(x, y)) => acc.appended(lean(prev)) -> Some(lean(prev))
    case ((acc, None), b @ O(x, y))       => acc.appended(bound)      -> Some(bound)
  }._1

def cycle(p: Platform): Platform =
  val n = tiltNorth(p)
  val w = tiltWest(n)
  val s = tiltSouth(w)
  tiltEast(s)

case class Cycle(it: Int, size: Int)

@tailrec
def cycleDetection(p: Platform, c: Map[Int, Int], i: Int): (Platform, Cycle) =
  val hash = p.bs.toSet.hashCode()
  c.get(hash) match
    case None    => cycleDetection(cycle(p), c + (hash -> i), i + 1)
    case Some(v) => p -> Cycle(i, i - v)

def part1(inp: String) =
  tiltNorth(Platform.fromString(inp)).score

def part2(inp: String) =

  val platform = Platform.fromString(inp)
  val total    = 1000000000
  val (p, c)   = cycleDetection(cycle(platform), HashMap.empty, 1)

  val remain = (total - c.it) % c.size
  (0 until remain).foldLeft(p) { case (a, _) => cycle(a) }.score

object Solution extends App:
  time(part1("day14.txt".live)) pipe println
  time(part2("day14.txt".live)) pipe println
