package aoc
package solutions
package day19

import scala.util.chaining.*
import scala.annotation.tailrec
import aoc.utils.time.time

enum ProcessingState:
  case GoTo(t: String)
  case Accepted
  case Rejected

object ProcessingState:
  def parse(inp: String) =
    inp match
      case "A" => Accepted
      case "R" => Rejected
      case o   => GoTo(o)

enum Opp:
  case LT(c: String, v: Int, r: ProcessingState)
  case GT(c: String, v: Int, r: ProcessingState)
  case DF(r: ProcessingState)

object Opp:
  def parse(inp: String): Opp =
    inp match
      case s"$c<$v:$i" => LT(c, v.toInt, ProcessingState.parse(i))
      case s"$c>$v:$i" => GT(c, v.toInt, ProcessingState.parse(i))
      case s"$i"       => DF(ProcessingState.parse(i))

case class Func(key: String, ops: List[Opp])

object Func:
  def parse(inp: String): Func =
    inp match
      case s"$n{$i}" =>
        val ops = i.split(",").map(Opp.parse).toList
        Func(n, ops)

case class Part(m: Map[String, Int]):
  lazy val score: Long = m.values.map(_.toLong).sum

object Part:
  def parse(inp: String): Part =
    inp match
      case s"{$a}" => Part(a.split(",").map { case s"$k=$v" => k -> v.toInt }.toMap)

@tailrec
def processPart(ops: List[Opp], p: Part): ProcessingState =
  ops match
    case hd :: tl =>
      hd match
        case Opp.LT(c, v, r) => if p.m(c) < v then r else processPart(tl, p)
        case Opp.GT(c, v, r) => if p.m(c) > v then r else processPart(tl, p)
        case Opp.DF(r)       => r
    // this will never happen, know that the last operator always evaluates to true
    case Nil => throw new Exception()

@tailrec
def collectAcceptedParts(
    queue: List[(Part, ProcessingState)],
    fs: Map[String, Func],
    acc: List[Part]
): List[Part] =
  queue match
    case (part, ProcessingState.Accepted) :: tl => collectAcceptedParts(tl, fs, part :: acc)
    case (part, ProcessingState.Rejected) :: tl => collectAcceptedParts(tl, fs, acc)
    case (part, ProcessingState.GoTo(id)) :: tl =>
      collectAcceptedParts((part -> processPart(fs(id).ops, part)) :: tl, fs, acc)
    case Nil => acc

case class R(s: Int, end: Int)

case class RangedPart(m: Map[String, R]):
  lazy val options: Long = m.values.map(r => (r.end - r.s + 1).toLong).reduce(_ * _)

object RangedPart:
  def full: RangedPart =
    RangedPart(
      Map(
        "x" -> R(1, 4000),
        "m" -> R(1, 4000),
        "a" -> R(1, 4000),
        "s" -> R(1, 4000)
      )
    )

def operate(p: RangedPart, o: Opp): (Option[(RangedPart, ProcessingState)], Option[(RangedPart)]) =
  o match
    case Opp.LT(c, v, r) =>
      val cr = p.m(c)
      // r -> 10, 100
      // v = 50
      // 10, 49 & 50, 100
      // if v is > the end, all are in range
      if v > cr.end then Some(p -> r) -> None
      // if v is <= the start, all are out of range
      else if v <= cr.s then None -> Some(p)
      else
        val inr = R(cr.s, v - 1)
        val oor = R(v, cr.end)
        Some(RangedPart(m = p.m + (c -> inr)) -> r) -> Some(RangedPart(m = p.m + (c -> oor)))

    case Opp.GT(c, v, r) =>
      val cr = p.m(c)
      // if v > end all are in range
      if v >= cr.end then Some(p, r) -> None
      // if v <= start all are out of range
      else if v < cr.s then None -> Some(p)
      else
        val oor = R(cr.s, v)
        val inr = R(v + 1, cr.end)
        Some(RangedPart(m = p.m + (c -> inr)) -> r) -> Some(RangedPart(m = p.m + (c -> oor)))

    case Opp.DF(r) => Some(p -> r) -> None

def processRangedPart(func: List[Opp], p: RangedPart): List[(RangedPart, ProcessingState)] =
  func match
    case h +: t =>
      operate(p, h) match
        case (Some(p, r), None)    => (p, r) :: Nil
        case (Some(p, r), Some(o)) => (p, r) :: processRangedPart(t, o)
        case (None, Some(o))       => processRangedPart(t, o)
        case (None, None)          => throw new Exception()
    case _ => Nil

def runRangedPart(
    queue: List[(RangedPart, ProcessingState)],
    fs: Map[String, Func],
    acc: List[RangedPart]
): List[RangedPart] =
  queue match
    case (p, ProcessingState.Accepted) :: tl => runRangedPart(tl, fs, p :: acc)
    case (p, ProcessingState.Rejected) :: tl => runRangedPart(tl, fs, acc)
    case (p, ProcessingState.GoTo(v)) :: tl =>
      runRangedPart(processRangedPart(fs(v).ops, p) ++ tl, fs, acc)

    case Nil => acc

def part1(inp: String) =
  val fs = inp.split("\n\n")(0).split("\n").map(Func.parse).map(f => f.key -> f).toMap
  val ps = inp.split("\n\n")(1).split("\n").map(Part.parse).toVector
  collectAcceptedParts(ps.map(p => p -> ProcessingState.GoTo("in")).toList, fs, List.empty).map(_.score).sum

def part2(inp: String) =
  val fs = inp.split("\n\n")(0).split("\n").map(Func.parse).map(f => f.key -> f).toMap
  val r  = RangedPart.full
  runRangedPart(List((r, ProcessingState.GoTo("in"))), fs, List.empty).map(_.options).sum

@main def main(): Unit =
  time(part1("day19.txt".live)) pipe println
  time(part2("day19.txt".live)) pipe println
