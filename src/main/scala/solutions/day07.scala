package aoc
package day07

import scala.util.chaining.*
import scala.annotation.tailrec

def strenght(inp: String) =
  inp.groupBy(identity).mapValues(s => s.length()).toMap.values.toList.sorted match
    case List(5)          => 1
    case List(1, 4)       => 2
    case List(2, 3)       => 3
    case List(1, 1, 3)    => 4
    case List(1, 2, 2)    => 5
    case List(1, 1, 1, 2) => 6
    case _                => 7

def upgraded(inp: String): String =
  val g  = inp.groupBy(identity).mapValues(s => s.length()).toMap
  val js = g.getOrElse('J', 0)
  if js == 0 || js == 5 then inp
  else inp.replace('J', g.filterKeys(_ != 'J').toMap.maxBy(_._2)._1)

case class Card(h: String, bid: Long)

object Card:
  def parse(inp: String): Card =
    inp match
      case s"$h $s" => Card(h, s.toLong)

def one(inp: List[String]) =
  lazy val worth = "AKQJT987654321".zipWithIndex.toMap

  def compare(l: Card, r: Card): Boolean =
    val leftStrength = strenght(l.h)
    val rightStength = strenght(r.h)

    if (leftStrength != rightStength) leftStrength < rightStength
    else
      l.h.toList.zip(r.h.toList).find((lc, rc) => lc != rc) match
        case None           => false
        case Some((lc, rc)) => worth(lc) < worth(rc)

  inp
    .map(Card.parse)
    .sortWith(compare)
    .reverse
    .zipWithIndex
    .map((c, s) => c.bid * (s + 1))
    .sum pipe println

def two(inp: List[String]) =
  lazy val worth = "AKQT987654321J".zipWithIndex.toMap

  def compare(l: Card, r: Card): Boolean =
    val leftStrength = strenght(upgraded(l.h))
    val rightStength = strenght(upgraded(r.h))

    if (leftStrength != rightStength) leftStrength < rightStength
    else
      l.h.toList.zip(r.h.toList).find((lc, rc) => lc != rc) match
        case None           => false
        case Some((lc, rc)) => worth(lc) < worth(rc)

  inp
    .map(Card.parse)
    .sortWith(compare)
    .reverse
    .zipWithIndex
    .map((c, s) => c.bid * (s + 1))
    .sum pipe println

object Solution extends App:
  one("day07.txt".live)
  two("day07.txt".live)
