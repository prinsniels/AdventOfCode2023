package aoc
package solutions
package day12

import scala.util.chaining.*

/**
  * Only 1 option due to the .
  * 1. .??.??.?##. 1,1,3
  * 
  * Two options ...
  * 2. * ??..??...?##. 1,1,3
  * 
  * Prune accordingly
  */
extension (b: Boolean)
    def toInt = if (b) 1 else  0

enum DF:
    case H, Q, D 

object DF:
    def apply(c: Char) = 
        c match
            case '?' => Q
            case '#' => H
            case '.' => D
        

type Record = Vector[DF]

object Record:
    def fromString(inp: String): Record = 
        inp.map(DF.apply).toVector


type Annotated = (Record, List[Int])

object Annotated:
    def fromString(inp: String): Annotated = inp match
        case s"$l $r" => Record.fromString(l) -> r.split(',').map(_.toInt).toList
    
    def fromStringMultiple(inp: String, i: Int): Annotated = inp match
        case s"$l $r" => Record.fromString(List.fill(i)(l).mkString("?")) -> List.fill(i)(r).mkString(",").split(',').map(_.toInt).toList

def allowsGroupOff(rec: Record, i: Int): Boolean =
    if rec.size < i then false 
    else if rec.size == i && !rec.contains(DF.D) then true
    else if rec.size > i &&  !rec.take(i).contains(DF.D) && rec(i) != DF.H then true
    else false


object solve:
    var cache: Map[(Record, List[Int]), Long] = Map.empty

    def eval(rec: Record, grp: List[Int]): Long =
        cache = Map.empty
        val r = work(rec, grp)
        cache = Map.empty
        r

    def work(rec: Record, grp: List[Int]): Long =
        cache.get((rec, grp)) match
            case Some(value) => value
            case None =>     
                if (grp.isEmpty) (!rec.contains(DF.H)).toInt.toLong
                else if (rec.isEmpty && grp.isEmpty) 1
                else if (grp.sum > rec.size) 0
                else rec.head match
                    case DF.H => 
                        val i = if allowsGroupOff(rec, grp.head) then work(rec.drop(grp.head + 1), grp.tail) else 0
                        cache = cache + ((rec, grp) -> i)
                        i
                    case DF.Q => 
                        val asH = if allowsGroupOff(rec, grp.head) then work(rec.drop(grp.head + 1), grp.tail) else 0
                        val asD = work(rec.tail, grp) 
                        val i = asH + asD
                        cache = cache + ((rec, grp) -> i)
                        i
                    case DF.D => 
                        val i = work(rec.tail, grp)
                        cache = cache + ((rec, grp) -> i)
                        i

def part1(inp: String) = 
    inp
      .split("\n")
      .map(Annotated.fromString)
      .map((l, r) => solve.eval(l, r))
      .sum


def part2(inp: String) = 
    inp
      .split("\n")
      .map(l => Annotated.fromStringMultiple(l, 5))
      .map((l, r) => solve.eval(l, r))
      .sum                 
    
object Solution extends App:
    part1("day12.txt".live) pipe println
    part2("day12.txt".live) pipe println
