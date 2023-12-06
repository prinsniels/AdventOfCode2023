package aoc
package day05

import scala.util.chaining.*
import scala.annotation.tailrec
import Math.{min, max}
import java.io.File

case class Trans(src: Long, dst: Long, r: Long):
  val srcUp = src + r

  def inRange(v: Long): Boolean =
    return src <= v && v < srcUp

  def eval(v: Long): Long =
    if inRange(v) then dst + v - src
    else v

  override def toString(): String =
    s"$dst $src $r [$src ${src + r})] "

case class Func(name: String, ts: List[Trans]):
  def eval(v: Long): Long =
    ts.find(_.inRange(v)) match
      case None    => v
      case Some(t) => t.eval(v)

  override def toString(): String =
    s"$name: \n" + ts.mkString("\n")

object Parsers:
  def bags(inp: String): List[BG.Bag] =
    ("""(\d+) (\d+)""".r)
      .findAllMatchIn(inp)
      .map(m => BG.Bag(m.group(1).toLong, m.group(1).toLong + m.group(2).toLong))
      .toList
      .asInstanceOf[List[BG.Bag]]

  def almanac(inp: List[String]): List[Func] =
    ("""(\D+) map: ([\d ]+)""".r)
      .findAllMatchIn(inp.tail.mkString(" "))
      .map { m =>
        Func(
          m.group(1).strip(),
          ("""(\d+) (\d+) (\d+)""".r)
            .findAllMatchIn(m.group(2))
            .map(g => Trans(g.group(2).toLong, g.group(1).toLong, g.group(3).toLong))
            .toList
        )
      }
      .toList

enum BG:
  case Node(bgs: List[BG])
  case Bag(s: Long, e: Long)

extension (b: BG)
  def split(t: Trans): BG =
    b match
      case BG.Node(bs) => BG.Node(bs.map(_.split(t)))
      case BG.Bag(s, e) => {
        val before: BG.Bag = BG.Bag(s, min(e, t.src))
        val inner: BG.Bag  = BG.Bag(max(s, t.src), min(e, t.srcUp))
        val after: BG.Bag  = BG.Bag(max(s, t.srcUp), e)

        val opt = List(before, inner, after).filter(b => b.e > b.s)

        if opt.length == 1 then opt.head
        else BG.Node(opt)
      }

  def mapBag(fb: BG.Bag => BG.Bag): BG =
    b match
      case BG.Node(bgs)     => BG.Node(bgs.map(_.mapBag(fb)))
      case b @ BG.Bag(s, e) => fb(b)

  def bags(): List[BG.Bag] =
    b match
      case BG.Node(bgs)     => bgs.flatMap(_.bags())
      case b @ BG.Bag(s, e) => List(b)

def one(inp: List[String]): Unit =
  val sds = ("""(\d+)""".r).findAllMatchIn(inp.head).map(m => m.group(1).toLong)
  val alm = Parsers.almanac(inp.tail)

  sds.map(s => alm.foldLeft(s) { case (v, f) => f.eval(v) }).min pipe println

def two(inp: List[String]): Unit =
  val bgs = Parsers.bags(inp.head)
  val alm = Parsers.almanac(inp.tail)

  def processBag(bg: BG, alm: List[Func]): Long =
    alm
      .foldLeft(bg) {
        case (b, f) => {
          val nwb = f.ts.foldLeft(b) { case (b, t) =>
            b.split(t)
          }
          nwb.mapBag(b => BG.Bag(f.eval(b.s), f.eval(b.e - 1) + 1))
        }
      }
      .bags()
      .map(_.s)
      .min

  bgs.map(b => processBag(b, alm)).min pipe println

@main def main() =
  val inp = "day05.txt".live

  one(inp)
  two(inp)
