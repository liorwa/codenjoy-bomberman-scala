package com.codenjoy.dojo.bomberman.client

import com.codenjoy.dojo.client.{Solver, WebSocketRunner}

class MySolver extends Solver[MyBoard] {
  var ticks: Int = 0

  override def get(b: MyBoard): String = {
    nextMove.toString
  }

  /*
      Implement your logic here
   */
  def nextMove: Action =
    Action(Right, BombBeforeMove)

}

object Main extends App {

  def secret = "3esw4i2sdvjhznqzakld"

  def code = "2616916371621770637"

  def url = "18.217.205.144:8080"

  override def main(args: Array[String]): Unit = {
    WebSocketRunner.runClient(s"http://$url/codenjoy-contest/board/player/$secret?code=$code", new MySolver, new MyBoard)
  }
}
