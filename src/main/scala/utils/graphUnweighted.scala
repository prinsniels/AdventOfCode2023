package aoc
package utils

import scala.collection.immutable.Queue

type Graph[A] = A => List[A]

def breathFirst[A](g: Graph[A], start: A): LazyList[(A, Option[A])] =
  def helper(q: Queue[A], visited: Set[A]): LazyList[(A, A)] = q match
    case h +: t =>
      val options = g(h).filterNot(visited)
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