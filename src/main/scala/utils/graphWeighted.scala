package aoc
package utils

import scala.collection.mutable.PriorityQueue

type Graph[A] = A => List[(A, Int)]

trait ShortestPath:
  def fromStartToEnd[A](g: Graph[A], start: A, end: A): Option[(Int, List[A])]

trait ShortestPathToAll:
  def fromStart[A](g: Graph[A], start: A): Map[A, Int]

  
object Dijkstra extends ShortestPath with ShortestPathToAll:

  override def fromStartToEnd[A](g: Graph[A], start: A, end: A): Option[(Int, List[A])] =
    takeUntil(
      prime(g, start),
      (_, t, _) => t == `end`,
      List.empty
    ) match
      case None => None
      case Some(value) => {
        Some(value.head._2 -> unwind(value.collect { case (a, _, Some(v)) => a -> v }.toMap, end).reverse)
      }

  override def fromStart[A](g: Graph[A], start: A): Map[A, Int] =
    prime(g, start).map(x => (x._1, x._2)).toMap

  private def unwind[A](as: Map[A, A], start: A): List[A] =
    as.get(start) match
      case Some(value) => start :: unwind(as, value)
      case None        => start :: Nil

  private def prime[A](g: Graph[A], start: A): LazyList[(A, Int, Option[A])] =
    solver(
      g,
      PriorityQueue[(A, Int)]((start, 0))(Ordering.by(t => -t._2)),
      Set.empty,
      Map(start -> 0),
      Map.empty
    )

  private def takeUntil[A](
      as: LazyList[(A, Int, Option[A])],
      pred: (A, Int, Option[A]) => Boolean,
      acc: List[(A, Int, Option[A])]
  ): Option[List[(A, Int, Option[A])]] =
    if (as.isEmpty) None
    else {
      val (a, i, oa) = as.head
      if pred(a, i, oa) then Some((a, i, oa) :: acc)
      else takeUntil(as.tail, pred, (a, i, oa) :: acc)
    }

  /** Simple dijkstra solver
    * @param g
    *   graph
    * @param q
    *   queue
    * @param v
    *   visited
    * @param d
    *   distance
    * @param p
    *   parent
    * @return
    */
  private def solver[T](
      g: T => List[(T, Int)],
      q: PriorityQueue[(T, Int)],
      v: Set[T],
      d: Map[T, Int],
      p: Map[T, T]
  ): LazyList[(T, Int, Option[T])] =
    if q.isEmpty then LazyList.empty
    else {

      /** (A, 1) (A, 1) (B, 2)
        */
      val (cur, curCst) = q.dequeue()

      if v(cur) then solver(g, q, v, d, p)
      else
        /** only take improvements on the current option */
        val options = g(cur)
          .map((nxt, nxtCst) => (nxt, curCst + nxtCst))
          .filterNot((nxt, cst) => d.getOrElse(nxt, cst) > cst)

        q.enqueue(options*)

        (cur, curCst, p.get(cur)) #:: solver(
          g,
          q,
          v + cur,
          d ++ options,
          p ++ options.map((nxt, _) => (nxt, cur))
        )
    }
