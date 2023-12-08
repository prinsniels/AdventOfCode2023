package aoc
package utils
package UnWeightedGraph

import scala.collection.immutable.Queue

extension [A](ll: LazyList[A])
  def takeUntil(pred: A => Boolean): LazyList[A] =
    ll match
      case h #:: t => if pred(h) then LazyList(h) else h #:: t.takeUntil(pred)
      case _       => LazyList.empty

/** A Graph can be brought back to a function that takes a Node and returns all reachable Nodes from
  * that Node
  */
type Graph[A] = A => List[A]

/** BreathFirst algorithm. The idea is to walk all nodes in a graph in a First In First Out manner,
  * in an unweighted graph this helps to ditermine the shortes path from a starting Node to all
  * other Nodes.
  *
  * I have opted for a lazy list to generate the result set, allowing me to do something with the
  * result set while it is being generated, or stop when then end is reached; 
  *   BreatFirst(...).takeUnitl(end)
  */
def breathFirst[A](graph: Graph[A], start: A): LazyList[(A, Option[A])] =
  def helper(q: Queue[A], visited: Set[A]): LazyList[(A, A)] = q match
    case h +: t =>
      val options = graph(h).filterNot(visited)
      options.map(x => (x, h)).to(LazyList) #::: helper(t ++ options, visited ++ options)
    case _ => LazyList.empty
  (start, None) #:: helper(Queue(start), Set(start)).map((c, f) => (c, Some(f)))

def depthFirst[A](g: Graph[A], start: A): LazyList[A] =
  def helper(q: List[A], frontier: Set[A]): LazyList[A] = q match
    case h :: t =>
      val options = g(h).filterNot(frontier)
      h #:: helper(options ++ t, frontier ++ options)
    case _ => LazyList.empty
  helper(List(start), Set(start))
