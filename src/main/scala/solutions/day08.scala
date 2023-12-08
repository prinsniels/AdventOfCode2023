package aoc
package solutions

import scala.util.chaining.*
import aoc.utils.{breathFirst, takeUntil}
import aoc.utils.lcm
import scala.annotation.tailrec

/** The problem requires us to count the amount of State changes from a Start State until a required
  * End State. State changes are a given set of instructions
  */


type State = String

// The trans
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


/** We transition from one state on the network to another, based on an input instruction. To set
  * this up we need to prime the Transition with a Network to operate on. This function creates the
  * actual Transition function, by priming it with the network map.
  */
def transitions(network: Map[String, Vector[String]]): Transition =
  (n, d) =>
    d match
      case aoc.solutions.Instr.GoLeft  => network(n)(0)
      case aoc.solutions.Instr.GoRight => network(n)(1)


/** Counting the steps until the end state is reached by checking if the end state has be reached, according to the `pred` function. If so we return the `count` otherwise we take another step by incrementing the count and transitioning from the current state to the next state. We recursively cal this function to find the amount of steps to the end state.
  */
@tailrec
def countStepsUntil(state: State, instrs: LazyList[Instr], trans: Transition, count: Int, pred: State => Boolean): Int =
  if pred(state) then count
  else countStepsUntil(trans(state, instrs.head), instrs.tail, trans, count + 1, pred)


def one(inp: List[String]) =
  val dirs    = Instr.parse(inp.head)
  val network = parseMap(inp.drop(2))
  val trans   = transitions(network)

  countStepsUntil("AAA", dirs, trans, 0, _ == "ZZZ") pipe println

def two(inp: List[String]) =
  val dirs    = Instr.parse(inp.head)
  val network = parseMap(inp.drop(2))
  val trans   = transitions(network)

  val starts: Set[State] = network.keySet.filter(_.endsWith("A"))

  starts
    .map(state => countStepsUntil(state, dirs, trans, 0, _.endsWith("Z")).toLong)
    .reduce(lcm) pipe println

object Solution extends App:
  one("day08.txt".live)
  two("day08.txt".live)

