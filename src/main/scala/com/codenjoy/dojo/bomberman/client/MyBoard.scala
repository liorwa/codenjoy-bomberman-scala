package com.codenjoy.dojo.bomberman.client

import java.util

import com.codenjoy.dojo.bomberman.model.Elements
import com.codenjoy.dojo.bomberman.model.Elements._
import com.codenjoy.dojo.client.AbstractBoard
import com.codenjoy.dojo.services.{Direction, Point}
import com.codenjoy.dojo.services.PointImpl.pt

import scala.collection.JavaConverters._
import scala.util.Try

class MyBoard extends AbstractBoard[Elements] {

  override def valueOf(c: Char): Elements = Elements.valueOf(c)

  override protected def inversionY(y: Int): Int = size - 1 - y

  override protected def withoutCorners: Boolean = true

  override def getAt(x: Int, y: Int): Elements = {
    if (x < 0 || y < 0 || x >= size || y >= size) return WALL
    super.getAt(x, y)
  }

  def getImpassable: Set[Point] = getMeatChoppers ++ getWalls ++ getBombs ++ getDestroyableWalls ++ getOtherBombermans


  //use this method as debug info - prints board information to the console
  override def toString: String =
    s"""
       |Board: $boardAsString
       |My bomberman at: $getMyBomberman
       |Other bombermans at: $getOtherBombermans
       |Meat choppers at: $getMeatChoppers
       |Destroyable walls at: $getDestroyableWalls
       |Bombs at: $getBombs
       |Blasts at: $getBlasts
       |Possible blasts at: $getFutureBlasts
     """.stripMargin

  def getMyBomberman: Point = get(BOMBERMAN, BOMB_BOMBERMAN, DEAD_BOMBERMAN).get(0)

  def getOtherBombermans: Set[Point] = get(OTHER_BOMBERMAN, OTHER_BOMB_BOMBERMAN, OTHER_DEAD_BOMBERMAN).asScala.toSet

  def isMyBombermanDead: Boolean = !get(DEAD_BOMBERMAN).isEmpty

  def getMeatChoppers: Set[Point] = get(MEAT_CHOPPER).asScala.toSet

  def getWalls: Set[Point] = get(WALL).asScala.toSet

  def getDestroyableWalls: Set[Point] = get(DESTROYABLE_WALL).asScala.toSet

  def getBombs: Set[Point] = {
    val result = get(BOMB_TIMER_1).asScala ++ get(BOMB_TIMER_2).asScala ++ get(BOMB_TIMER_3).asScala ++
      get(BOMB_TIMER_4).asScala ++ get(BOMB_TIMER_5).asScala ++
      get(BOMB_BOMBERMAN).asScala ++ get(OTHER_BOMB_BOMBERMAN).asScala
    result.toSet
  }

  def getBlasts: util.Collection[Point] = get(BOOM)

  //todo remove blocked neighbours
  def addPointNeighbours(point: Point, length: Int = 3): Set[Point] =
    (1 to length).flatMap(i => Set(
      pt(point.getX - i, point.getY),
      pt(point.getX + i, point.getY),
      pt(point.getX, point.getY - i),
      pt(point.getX, point.getY + i)
    )).filter(p => !p.isOutOf(size) && !getWalls.contains(p)).toSet


  def getFutureBlasts: Set[Point] =
    getBombs.flatMap(bomb => getFutureBlasts(bomb))
 
  def getFutureBlasts(bomb: Point): Set[Point] =
    addPointNeighbours(bomb) ++ Set(bomb)


  def isBarrierAt(x: Int, y: Int): Boolean = getImpassable.contains(pt(x, y))

  def isBarrierAt(point: Point): Boolean = isBarrierAt(point.getX, point.getY)

  def nextMoveToPoint(from: Point, to: Point): Option[Direction] = closestPathToPoint(from ,to).flatMap(_.headOption)

  def closestPathToPoint(from: Point, to: Point): Option[Seq[Direction]] = {
    def bfs(accum: Seq[Direction], visited: Set[Point], current: Point): Option[Seq[Direction]] = {
      if (current == to) Some(accum)
      else {
        Try{Direction.onlyDirections().asScala.map(direction => {
          val newX = direction.changeX(current.getX)
          val newY = direction.changeX(current.getY)
          val newPoint = pt(newX, newY)
          if (!isBarrierAt(newPoint) && !visited.contains(newPoint))
            bfs(accum ++ Seq(direction), visited ++ Set(newPoint), newPoint).getOrElse(Seq.empty)
          else Seq.empty
        }).filter(_ != Seq.empty).minBy(_.size)}.toOption
      }
    }
    bfs(Seq.empty, Set(from), from)
  }

  def getBlastWithBombAndTimer: Seq[BombWithBlasts] = getBombs.map(b => BombWithBlasts(b, addPointNeighbours(b), getAt(b).ch() - '0')).toSeq

  def getDistanceToPoint(from: Point, to: Point): Int = closestPathToPoint(from, to).size

  def getNearestBomberman(from: Point): Option[Point] = getOtherBombermans.toSeq match {
    case Nil => None
    case a@_ => Some(a.minBy(getDistanceToPoint(from, _)))
  }

  def getNearestMeatChopper(from: Point): Option[Point] = getMeatChoppers.toSeq match {
    case Nil => None
    case a@_ => Some(a.minBy(getDistanceToPoint(from, _)))
  }
}

case class BombWithBlasts(bombLocation: Point, possibleBlasts: Set[Point], timeToBlast: Int)
