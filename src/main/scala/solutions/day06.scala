package aoc
package day06

import scala.util.chaining.*
import scala.annotation.tailrec

def inRangeOff(t: Long, d: Long)(h: Long): Boolean =
  val travel = t - h
  val dist   = travel * h
  dist >= d

def one() =
  val inp = List((53, 313), (89, 1090), (76, 1214), (98, 1201))

  inp.map((a, b) => (0 to a).count(inRangeOff(a, b))).fold(1)(_ * _) pipe println

def two() =
  def foo(a: Long, c: Long): Long =
    if (a == 0) c
    else if (inRangeOff(53897698L, 313109012141201L)(a)) foo(a - 1, c + 1)
    else foo(a - 1, c)

  foo(53897698L, 0) pipe println

object Solution extends App:
  one()
  two()
