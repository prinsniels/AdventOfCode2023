package aoc
package day01

import scala.util.chaining.*
import aoc.utils.lcm
import aoc.utils.depthFirst
import aoc.utils.breathFirst
import aoc.board.Vec
import aoc.utils.Dijkstra


@main def main(): Unit =
    println("hello".live)
    
    val g: Vec => List[Vec] = inp => List(Vec(0, 1), Vec(1, 0)).map(_ + inp)

    breathFirst(g, Vec(0, 0)).take(5).toList pipe println
    depthFirst(g, Vec(0, 0)).take(5).toList pipe println

    val wg = Map(
        0 -> List((1, 1), (2, 1)),
        1 -> List((2, 4)),
        2 -> List((3, 2)),
        3 -> List()
    )

    Dijkstra.fromStart(wg, 0) pipe println
    Dijkstra.fromStartToEnd(wg, 0, 3) pipe println
    Dijkstra.fromStartToEnd(wg, 0, 6) pipe println
    Dijkstra.fromStartToEnd(wg, 0, 0) pipe println