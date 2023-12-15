package aoc
package solutions
package day15

import scala.util.chaining.*

case class Lens(label: String, v: Int)

enum Opp:
  val label: String

  case AddLens(label: String, v: Int)
  case RemoveLens(label: String, v: Int)
  case RemoveAllOf(label: String)

object Opp:
  def fromString(inp: String) =
    inp match
      case s"$b=$v"            => Opp.AddLens(b, v.toInt)
      case s"$b-$v" if v != "" => Opp.RemoveLens(b, v.toInt)
      case s"$b-"              => Opp.RemoveAllOf(b)

def codeToInt(inp: String) =
  inp.map(_.toInt).foldLeft(0) { case (acc, c) =>
    ((acc + c) * 17) % 256
  }

def addToBox(box: Vector[Lens], l: Lens): Vector[Lens] =
  box match
    case h +: t if h.label == l.label => l +: t
    case h +: t if h.label != l.label => h +: addToBox(t, l)
    case _                            => Vector(l)

def focussingPower(b: Int, s: Int, l: Int): Long =
  b * s * l

def generateBoxes(ops: Iterable[Opp]): Map[Int, Vector[Lens]] =
  ops.foldLeft(Map.empty[Int, Vector[Lens]]) { case (bxs, instr) =>
    val bxCode = codeToInt(instr.label)
    val box    = bxs.getOrElse(bxCode, Vector.empty)
    instr match
      case Opp.AddLens(b, v) =>
        bxs + (bxCode -> addToBox(box, Lens(b, v)))

      case Opp.RemoveLens(b, v) =>
        bxs + (bxCode -> box.filterNot(i => i.label == b && i.v == v))

      case Opp.RemoveAllOf(b) =>
        bxs + (bxCode -> box.filterNot(i => i.label == b))
  }

def part1(inp: String) =
  inp.split(",").map(codeToInt).reduce(_ + _)

def part2(inp: String) =
  val ops = inp.split(",").map(Opp.fromString)
  generateBoxes(ops)
    .flatMap((idx, box) => box.zipWithIndex.map((l, s) => focussingPower(idx + 1, s + 1, l.v)))
    .sum

object day15 extends App:
  part1("day15.txt".live) pipe println
  part2("day15.txt".live) pipe println
