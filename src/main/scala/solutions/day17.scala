package aoc
package solutions
package day17

import scala.util.chaining.*
import scala.collection.mutable.PriorityQueue
import scala.annotation.tailrec
import aoc.utils.time.time

enum Dir(val x: Int, val y: Int):
  case N extends Dir(0, -1)
  case E extends Dir(1, 0)
  case S extends Dir(0, 1)
  case W extends Dir(-1, 0)

extension (d: Dir)
  def turnLeft: Dir = d match
    case Dir.N => Dir.W
    case Dir.E => Dir.N
    case Dir.S => Dir.E
    case Dir.W => Dir.S

  def turnRight: Dir = d match
    case Dir.N => Dir.E
    case Dir.E => Dir.S
    case Dir.S => Dir.W
    case Dir.W => Dir.N

case class City(v: Vector[Vector[Int]]):
  lazy val maxY = v.size - 1
  lazy val maxX = v.head.size - 1

  def contains(b: Block): Boolean =
    b.x >= 0 && b.y >= 0 && b.x <= maxX && b.y <= maxY

  def getCost(b: Block): Int =
    if contains(b) then v(b.y)(b.x) else 0

object City:
  def parse(inp: String): City =
    City(inp.split("\n").toVector.map(_.map(_.toString.toInt).toVector))

case class Block(x: Int, y: Int):
  def path(d: Dir, s: Int): Vector[Block] =
    (1 to s).map(m => Block(x + d.x * m, y + d.y * m)).toVector

  def move(d: Dir): Block =
    path(d, 1).last

case class Crucible(p: Block, steps: Int, dir: Dir)

type Graph[T] = T => List[(T, Int)]

def graphBuilder(c: City, mn: Int, mx: Int): Graph[Crucible] =
  cr =>
    // if turning, at least `mn` steps
    // if forward, no more than `mx` steps
    val lp   = cr.p.path(cr.dir.turnLeft, mn)
    val lcst = lp.map(c.getCost).sum
    val lc   = Crucible(lp.last, mn, cr.dir.turnLeft) -> lcst

    val rp   = cr.p.path(cr.dir.turnRight, mn)
    val rcst = rp.map(c.getCost).sum
    val rc   = Crucible(rp.last, mn, cr.dir.turnRight) -> rcst

    val fcst = c.getCost(cr.p.move(cr.dir))
    val fwd  = Crucible(cr.p.move(cr.dir), cr.steps + 1, cr.dir) -> fcst

    List(lc, rc, fwd)
      .filter((cur, _) => c.contains(cur.p))
      .filterNot((cur, _) => cur.steps < mn || cur.steps > mx)

/** Simple dijkstra solver
  */
def solver[T](
    g: T => List[(T, Int)],
    q: PriorityQueue[(T, Int)],
    v: Set[T],
    d: Map[T, Int],
    p: Map[T, T]
): LazyList[(T, Int, Option[T])] =
  if q.isEmpty then LazyList.empty
  else {

    /** (A, 1) (A, 1) (B, 2)
      */
    val (cur, curCst) = q.dequeue()

    if v(cur) then solver(g, q, v, d, p)
    else
      /** only take improvements on the current option */
      val options = g(cur)
        .map((nxt, nxtCst) => (nxt, curCst + nxtCst))
        .filterNot((nxt, cst) => d.getOrElse(nxt, cst) > cst)

      q.enqueue(options*)

      (cur, curCst, p.get(cur)) #:: solver(
        g,
        q,
        v + cur,
        d ++ options,
        p ++ options.map((nxt, _) => (nxt, cur))
      )
  }

@tailrec
def find(ll: LazyList[(Crucible, Int, Option[Crucible])], b: Block): Int =
  val (cr, cst, prev) = ll.head
  if cr.p == b then cst
  else find(ll.tail, b)

def part1(inp: String) =
  val city  = City.parse("day17.txt".live)
  val graph = graphBuilder(city, 1, 3)
  val init  = Crucible(Block(0, 0), 0, Dir.E)

  given Ordering[(Crucible, Int)] = Ordering.by(-_._2)
  val solve = solver(graph, PriorityQueue((init, 0)), Set.empty, Map(init -> 0), Map.empty)
  find(solve, Block(city.maxX, city.maxY))

def part2(inp: String) =
  val city  = City.parse("day17.txt".live)
  val graph = graphBuilder(city, 4, 10)
  val init  = Crucible(Block(0, 0), 0, Dir.E)

  given Ordering[(Crucible, Int)] = Ordering.by(-_._2)
  val solve = solver(graph, PriorityQueue((init, 0)), Set.empty, Map(init -> 0), Map.empty)
  find(solve, Block(city.maxX, city.maxY))

@main def main(): Unit =
  time(part1("day17.txt".sample)) pipe println
  time(part2("day17.txt".sample)) pipe println
