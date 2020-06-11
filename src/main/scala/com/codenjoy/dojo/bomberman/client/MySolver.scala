package com.codenjoy.dojo.bomberman.client

import com.codenjoy.dojo.client.{Solver, WebSocketRunner}
import com.codenjoy.dojo.services.{Direction, Point, PointImpl}

class MySolver extends Solver[MyBoard] {
  val MIN_DIST_TO_VICTIM = 4
  var ticks: Int = 0

  override def get(b: MyBoard): String =
    nextMove(b).toString

  def nextMove(b: MyBoard): Action = {
    attack(b)
  }

  private def attack(b: MyBoard): Action = {
    val currentPoint = b.getMyBomberman
    findsNearestAvailableBomberman(b).map { victim ⇒
      val nextMoveTowardsVictim = b.nextMoveToPoint(currentPoint, victim, false).get
      val nextPointTowardsVictim = Helpers.movePoint(nextMoveTowardsVictim, currentPoint)
      val distToVictim = b.getEuclideanDistanceToPoint(currentPoint, victim)
      val putBomb =
        canKill(b, victim, nextPointTowardsVictim, distToVictim) ||
        distToVictim > 5
      if (shouldNotGoThere(b, nextPointTowardsVictim)) {
        debug("escapeIfNeeded")
        escapeIfNeeded(b)
      }
      else {
        Action(toMove(nextMoveTowardsVictim), if (putBomb) BombAfterMove else NoBomb)
      }
    }.getOrElse {
      debug("Suicide")
      Action(Stay, BombBeforeMove)
    }
  }

  private def canKill(b: MyBoard, victim: Point, nextPointTowardsVictim: Point, distToVictim: Int) = {
    distToVictim < MIN_DIST_TO_VICTIM &&
      b.getFutureBlasts2(nextPointTowardsVictim).contains(victim)
  }

  private def shouldNotGoThere(b: MyBoard, point: Point) =
    b.getFutureBlasts.contains(point)

  // If I'm not standing on a blast - stay
  // Else - Escape the bomb
  private def escapeIfNeeded(b: MyBoard): Action = {
    val currentPoint = b.getMyBomberman
    val futureBlasts = b.getFutureBlasts
    val impassable = b.getImpassable
    val x = currentPoint.getX
    val y = currentPoint.getY
    if (futureBlasts.contains(currentPoint)) {
      val points = (for {
        xToCheck ← x - 2 to x + 2
        yToCheck ← y - 2 to y + 2
      } yield {
        val point = PointImpl.pt(xToCheck, yToCheck)
        val canGoThere = !futureBlasts.contains(point) && !impassable.contains(point)
        if (canGoThere) {
          Some(point)
        } else {
          None
        }
      }).flatten
        val direction = points
        .find(p ⇒ b.nextMoveToPoint(currentPoint, p, true).isDefined)
        .flatMap(p ⇒ b.nextMoveToPoint(currentPoint, p, true))

      Action(direction.map(x ⇒ toMove(x)).getOrElse(Stay), NoBomb)
    } else
      Action(Stay, NoBomb)
  }

  def findsNearestAvailableBomberman(b: MyBoard): Option[Point] = {
    val currentPoint = b.getMyBomberman
    b.getOtherBombermans.toList
      .sortBy(point ⇒ b.getEuclideanDistanceToPoint(currentPoint, point))
      .find(point ⇒ b.nextMoveToPoint(currentPoint, point, false).isDefined)
  }

  def toMove(direction: Direction): Move =
    direction match {
      case Direction.LEFT ⇒ Left
      case Direction.RIGHT ⇒ Right
      case Direction.UP ⇒ Up
      case Direction.DOWN ⇒ Down
    }

  def debug(str: String) =
    println("!!!!!!" + str)
}

object Main extends App {
//http://3.133.109.198:8080/codenjoy-contest/board/player/oefg7mda3u2oncmx5t2o?code=1312199000384230733&gameName=bomberman

  def secret = "oefg7mda3u2oncmx5t2o"

  def code = "1312199000384230733"

  def url = "3.133.109.198:8080"

  override def main(args: Array[String]): Unit = {
    WebSocketRunner.runClient(s"http://$url/codenjoy-contest/board/player/$secret?code=$code", new MySolver, new MyBoard)
  }
}
