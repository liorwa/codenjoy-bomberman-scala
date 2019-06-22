package com.codenjoy.dojo.bomberman.client

import com.codenjoy.dojo.client.{Solver, WebSocketRunner}
import com.codenjoy.dojo.services.{Direction, Point}

import scala.util.Random

class MySolver extends Solver[MyBoard] {
  var ticks: Int = 0

  override def get(b: MyBoard): String = {
    ticks += 1
    val myPos = b.getMyBomberman
    val toMeatChopper: Direction = b.nextMoveToPoint(myPos, b.getNearestMeatChopper(myPos).getOrElse(myPos)).getOrElse(Direction.STOP)
    val toBomberman = b.nextMoveToPoint(myPos, b.getNearestBomberman(myPos).getOrElse(myPos)).getOrElse(Direction.STOP)
    if (ticks == 0) {
      return "ACT"
    }
    (List(toMeatChopper, toBomberman) ++ Random.shuffle((0 to 3).toList).map(Direction.valueOf)).filter(_ != Direction.STOP).foreach(dir =>
      if (isMoveRight(b, dir)) {
        return dir.ACT(true)
      })

    "ACT"
  }

  def getCoords(b: MyBoard, direction: Direction): Point = {
    val position = b.getMyBomberman
    direction.change(position)
  }

  def isMoveable(b: MyBoard, direction: Direction): Boolean = {
    val newPosition = getCoords(b, direction)
    !(b.getWalls.contains(newPosition) || b.getImpassable.contains(newPosition) || b.getBombs.contains(newPosition) || b.getDestroyableWalls.contains(newPosition) || b.getMeatChoppers.contains(newPosition))
  }

  def isMoveRight(b: MyBoard, d: Direction): Boolean = {
    val newPosition = getCoords(b, d)
    (!b.getBlastWithBombAndTimer.filter(_.timeToBlast <= 1).map(_.possibleBlasts).exists(_.contains(newPosition))) && isMoveable(b, d)
  }
}

object Main extends App {

  def email = "ol@gmail.com"

  def code = "541537394769019901"

  def url = "34.67.67.45:8080"

  override def main(args: Array[String]): Unit = {
    WebSocketRunner.runClient(s"http://$url/codenjoy-contest/board/player/$email?code=$code", new MySolver, new MyBoard)
  }
}
