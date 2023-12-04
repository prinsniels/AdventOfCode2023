package aoc
package day04

import scala.util.chaining.*
import aoc.board.Vec
import scala.annotation.tailrec

case class Card(id: Int, left: List[Int], right: List[Int]):
  val matches = left.intersect(right).length

  val score: Long =
    matches match
      case 0 => 0
      case x => doubleScore(x - 1, 1)

@tailrec
def doubleScore(x: Int, cur: Long): Long =
  if x == 0 then cur
  else doubleScore(x - 1, cur * 2)

@tailrec
def playGame(cur: Int, deck: Map[Int, Card], acc: Map[Int, Long]): Long =
  val totalCards = deck.size 
  if cur > totalCards then acc.values.sum
  else {
    val ins = acc(cur)
    val won = (0 until deck(cur).matches).map(_ + 1 + cur)
    val ext = won.filter(_ <= totalCards).map(idx => idx -> (acc(idx) + ins))
    playGame(cur + 1, deck, acc ++ ext)
  }

object Card:
  def apply(inp: String): Card =
    inp match
      case s"Card $i: $l|$r" => Card(i.strip().toInt, toNumbers(l), toNumbers(r))

  def toNumbers(i: String): List[Int] =
    ("""(\d+)""".r).findAllIn(i).map(_.strip().toInt).toList

def one(inp: List[String]): Long =
  inp.map(Card.apply).map(_.score).sum

def two(inp: List[String]): Long =
  val deck = inp.map(Card.apply).map(c => c.id -> c).toMap
  val acc  = (1 to deck.keySet.max).map(_ -> 1L).toMap
  playGame(1, deck, acc)

object Solution extends App:

  one("day04.txt".live) pipe println
  two("day04.txt".live) pipe println
