package aoc
package day02

import scala.util.chaining.*

case class Game(id: Int, subs: List[Map[String, Int]])

object Game:
  def parse(inp: String): Game =
    inp match
      case s"Game $i:$r" => Game(i.toInt, getGameSets(r))

  def getShown(inp: String): (String, Int) =
    inp match
      case s"$s $c" => (c, s.toInt)

  def getGameSets(inp: String): List[Map[String, Int]] =
    inp.split(';').map(_.split(',').map(_.strip()).map(getShown).toMap).toList

def setPossible(inGame: Map[String, Int], set: Map[String, Int]): Boolean =
  // for a set tobe possible, all the found chips must be in the game
  set.forall { case (cf, af) => inGame.getOrElse(cf, af) >= af }

def gamePossible(inGame: Map[String, Int], game: Game): Boolean =
  game.subs.forall(set => setPossible(inGame, set))

def gameMinimum(game: Game): Map[String, Int] =
  game.subs.foldLeft(Map.empty) { case (cur, set) =>
    set.foldLeft(cur) { case (cur, (sc, sv)) =>
      cur + (sc -> Math.max(cur.getOrElse(sc, sv), sv))
    }
  }

def setPower(set: Map[String, Int]): Int =
  set.getOrElse("red", 0) * set.getOrElse("green", 0) * set.getOrElse("blue", 0)

object Solution extends App:
  def one(inp: String) =
    val specGamePos = game =>
      gamePossible(
        Map(
          "red"   -> 12,
          "green" -> 13,
          "blue"  -> 14
        ),
        game
      )

    inp.split("\n").map(Game.parse).filter(specGamePos).collect(_.id).sum

  def two(inp: String) =
    inp.split("\n").map(Game.parse).map(gameMinimum).map(setPower).map(_.toLong).sum

  one("day02.txt".live) pipe println
  two("day02.txt".live) pipe println
