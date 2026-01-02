package net.ivoah.letsgetmarried

import controller.Endpoints
import org.rogach.scallop.*
import net.ivoah.vial.*

@main
def main(args: String*): Unit = {
  class Conf(args: Seq[String]) extends ScallopConf(args) {
    val host: ScallopOption[String] = opt[String](default = Some("127.0.0.1"))
    val port: ScallopOption[Int] = opt[Int](default = Some(4269))
    val socket: ScallopOption[String] = opt[String]()
    val verbose: ScallopOption[Boolean] = opt[Boolean](default = Some(false))

    conflicts(socket, List(host, port))
    verify()
  }

  val conf = Conf(args)
  implicit val logger: String => Unit = if (conf.verbose()) println else (msg: String) => ()
  val endpoints = Endpoints()
  val server = conf.socket.toOption match {
    case Some(path) =>
      println(s"Using unix socket: $path")
      Server(endpoints.router, path)
    case None =>
      println(s"Using host/port: ${conf.host()}:${conf.port()}")
      Server(endpoints.router, (conf.host(), conf.port()))
  }
  server.serve()
}
