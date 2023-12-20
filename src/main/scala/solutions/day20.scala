package aoc
package solutions
package day20

import scala.util.chaining.*
import aoc.utils.time.time
import scala.annotation.tailrec
import aoc.utils.lcm

enum Pulse(val orig: String, val dest: String):
  case High(o: String, a: String) extends Pulse(o, a)
  case Low(o: String, a: String)  extends Pulse(o, a)

sealed trait Module:
  val id: String
  val destinations: List[String]
  def process(pulse: Pulse): (Module, List[Pulse])

case class Broadcast(id: String, destinations: List[String]) extends Module:
  def process(pulse: Pulse): (Module, List[Pulse]) =
    this -> destinations.map(dest =>
      pulse match
        case Pulse.High(o, d) => Pulse.High(id, dest)
        case Pulse.Low(o, d)  => Pulse.Low(id, dest)
    )

case class FlipFlopOn(id: String, destinations: List[String]) extends Module:
  def process(pulse: Pulse): (Module, List[Pulse]) =
    pulse match
      case Pulse.High(orig, dest) => this -> List.empty
      case Pulse.Low(orig, dest) =>
        FlipFlopOff(id, destinations) -> destinations.map(dest => Pulse.Low(id, dest))

case class FlipFlopOff(id: String, destinations: List[String]) extends Module:
  def process(pulse: Pulse): (Module, List[Pulse]) =
    pulse match
      case Pulse.High(orig, dest) => this -> List.empty
      case Pulse.Low(orig, dest) =>
        FlipFlopOn(id, destinations) -> destinations.map(dest => Pulse.High(id, dest))

case class Conjunction(id: String, destinations: List[String], mem: Map[String, Pulse])
    extends Module:
  def process(pulse: Pulse): (Module, List[Pulse]) =
    val nwMem = mem + (pulse.orig -> pulse)
    if nwMem.values.forall(_.isInstanceOf[Pulse.High]) then
      copy(mem = nwMem)    -> destinations.map(dest => Pulse.Low(id, dest))
    else copy(mem = nwMem) -> destinations.map(dest => Pulse.High(id, dest))

case class Sink(id: String) extends Module:
  val destinations: List[String]                   = List.empty
  def process(pulse: Pulse): (Module, List[Pulse]) = (this, List.empty)

type Network = Map[String, Module]

object Module:
  def parse(inp: String): Module =
    inp match
      case s"broadcaster -> $d" => Broadcast("broadcaster", d.split(",").map(_.strip).toList)
      case s"&${i} -> $d"       => Conjunction(i, d.split(",").map(_.strip).toList, Map.empty)
      case s"%${i} -> $d"       => FlipFlopOff(i, d.split(",").map(_.strip).toList)

object Network:
  def parse(inp: String) =
    val mods = inp.split("\n").map(Module.parse)
    // update Conjunction mods
    val updated = mods
      .filter(_.isInstanceOf[Conjunction])
      .map(c =>
        Conjunction(
          c.id,
          c.destinations,
          mem = mods
            .filter(_.destinations.contains(c.id))
            .map(_.id)
            .map(id => id -> Pulse.Low(id, c.id))
            .toMap
        )
      )
      .toVector

    val primed_map =
      (updated ++ mods.filter(!_.isInstanceOf[Conjunction])).map(m => m.id -> m).toMap
    // add all keys that don't have a target to the nodes
    val sinks = mods
      .flatMap(_.destinations)
      .toSet
      .filterNot(primed_map.keySet)
      .toVector
      .map(id => id -> Sink(id))
      .toMap
    primed_map ++ sinks

@tailrec
def run(nw: Network, signals: Vector[Pulse], low: Int, high: Int): (Network, Int, Int) =
  signals match
    case h +: t =>
      val (mod, res) = nw(h.dest).process(h)
      run(
        nw + (mod.id -> mod),
        t ++ res,
        if h.isInstanceOf[Pulse.Low] then low + 1 else low,
        if h.isInstanceOf[Pulse.High] then high + 1 else high
      )
    case _ => (nw, low, high)

def part1(inp: String) =
  val nw   = Network.parse(inp) + ("output" -> Sink("output"))
  val init = Pulse.Low("button", "broadcaster")

  val (a, b, c) = (0 until 1000).foldLeft(nw, 0, 0) { case ((n, l, h), _) =>
    run(n, Vector(init), l, h)
  }
  b.toLong * c.toLong

@tailrec
def run2(
    nw: Network,
    signals: Vector[Pulse],
    targets: Map[String, Long],
    i: Int
): (Network, Map[String, Long]) =
  signals match
    case h +: t =>
      val (mod, res) = nw(h.dest).process(h)
      if h.isInstanceOf[Pulse.High] && targets.keySet(h.orig) && (targets(h.orig) == 0) then
        run2(nw + (mod.id    -> mod), t ++ res, targets + (h.orig -> i), i)
      else run2(nw + (mod.id -> mod), t ++ res, targets, i)
    case _ => (nw, targets)

def part2(inp: String) =
  val nw   = Network.parse(inp) + ("output" -> Sink("output"))
  val init = Pulse.Low("button", "broadcaster")
  val targets = nw
    .filter((k, v) => v.destinations.contains("rx"))
    .flatMap((_, c) => c.asInstanceOf[Conjunction].mem.keys.toVector)
    .map(_ -> 0L)
    .toMap

  def search(nw: Network, targets: Map[String, Long], i: Int): Map[String, Long] =
    val (n, t) = run2(nw, Vector(init), targets, i)
    if t.forall((k, v) => v > 0) then t
    else search(n, t, i + 1)

  search(nw, targets, 1).map((_, v) => v).reduce(lcm)

object Solution extends App:
  // time(part1("day20.txt".live)) pipe println
  time(part2("day20.txt".live)) pipe println
