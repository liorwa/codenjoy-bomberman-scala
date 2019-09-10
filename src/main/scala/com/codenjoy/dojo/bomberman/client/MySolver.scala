package com.codenjoy.dojo.bomberman.client

import com.codenjoy.dojo.client.{Solver, WebSocketRunner}

class MySolver extends Solver[MyBoard] {
  var ticks: Int = 0

  override def get(b: MyBoard): String = {
    "ACT"
  }
}

object Main extends App {

  def email = "ol@gmail.com"

  def code = "541537394769019901"

  def url = "18.217.205.144:8080"

  override def main(args: Array[String]): Unit = {
    WebSocketRunner.runClient(s"http://$url/codenjoy-contest/board/player/$email?code=$code", new MySolver, new MyBoard)
  }
}
