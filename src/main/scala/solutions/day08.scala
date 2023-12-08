package aoc
package solutions

import scala.util.chaining.*
import aoc.utils.{breathFirst, takeUntil}
import aoc.utils.lcm
import scala.annotation.tailrec

type State = String

type Transition = (State, Instr) => State

enum Instr:
  case GoLeft, GoRight

object Instr:
  def parse(inp: String): LazyList[Instr] =
    inp
      .map { c =>
        c match
          case 'L' => Instr.GoLeft
          case 'R' => Instr.GoRight
      }
      .to(LazyList) #::: Instr.parse(inp)

def parseMap(inp: List[String]): Map[String, Vector[String]] =
  inp.map { i =>
    i match
      case s"$a = ($b, $c)" => (a -> Vector(b, c))
  }.toMap

def transitions(network: Map[String, Vector[String]]): Transition =
  (n, d) =>
    d match
      case aoc.solutions.Instr.GoLeft  => network(n)(0)
      case aoc.solutions.Instr.GoRight => network(n)(1)

@tailrec
def countStepsUntil(state: State, instrs: LazyList[Instr], trans: Transition, count: Int, pred: State => Boolean): Int =
  if pred(state) then count
  else countStepsUntil(trans(state, instrs.head), instrs.tail, trans, count + 1, pred)


def one(inp: List[String]) =
  val instructions  = Instr.parse(inp.head)
  val network       = parseMap(inp.drop(2))
  val trans         = transitions(network)

  countStepsUntil("AAA", instructions, trans, 0, _ == "ZZZ") pipe println

def two(inp: List[String]) =
  val instructions  = Instr.parse(inp.head)
  val network       = parseMap(inp.drop(2))
  val trans         = transitions(network)

  val starts: Set[State] = network.keySet.filter(_.endsWith("A"))

  def lcm(a: Long, b: Long): Long =
    a * b / gcd(a, b)
  
  def gcd(a: Long, b: Long): Long =
    if b == 0 then a else gcd(b, a % b)

  starts
    .map(state => countStepsUntil(state, instructions, trans, 0, _.endsWith("Z")).toLong)
    .reduce(lcm) pipe println

object Solution extends App:
  one("day08.txt".live)
  two("day08.txt".live)

