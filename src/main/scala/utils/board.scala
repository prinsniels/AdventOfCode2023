package aoc
package utils
package board

import scala.util.chaining.*

type Board[A] = Map[Vec, A]

case class Vec(x: Int, y: Int) extends AnyRef:
  def +(o: Vec): Vec =
    Vec(x + o.x, y + o.y)

extension (raw: List[String])
  def board[A](ca: Char => A): Board[A] = {
    for {
      (line, y) <- raw.zipWithIndex
      (char, x) <- line.zipWithIndex
    } yield (Vec(x, y), ca(char))
  }.toMap

extension [A](b: Board[A])
  def minX: Int = b.keys.map(_.x).min
  def maxX: Int = b.keys.map(_.x).max
  def minY: Int = b.keys.map(_.y).min
  def maxY: Int = b.keys.map(_.y).max

  def show: Unit =
    (b.minY to b.maxY).foreach { y =>
      (b.minX to b.maxX).map(x => b.getOrElse(Vec(x, y), " ")).mkString pipe println
    }