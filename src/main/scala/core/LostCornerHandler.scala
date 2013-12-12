package core

import scalax.io.Resource

object LostCornerHandler {
  
  def countLostCorner(filename: String) = {
    val res = Resource.fromFile(filename).lines().map { line =>
      val matrix = Array.ofDim[Double](122, 105)
      line.split(" ").tail.foreach { str =>
        val lr = str.split(":")
        val index = lr.head.toInt - 1
        val value = lr.last.toDouble
        matrix(index / 105)(index % 105) = value
      }
      val lt = (for (i <- 0 to 60; j <- 0 to 51) yield matrix(i)(j)).find(_ > 0).isEmpty
      val rt = (for (i <- 0 to 60; j <- 53 to 104) yield matrix(i)(j)).find(_ > 0).isEmpty
      val rb = (for (i <- 61 to 121; j <- 53 to 104) yield matrix(i)(j)).find(_ > 0).isEmpty
      val lb = (for (i <- 61 to 121; j <- 0 to 51) yield matrix(i)(j)).find(_ > 0).isEmpty
      if (Seq(lt, rt, rb, lb).count(b => b) > 1) 0
      else if (lt) 1
      else if (rt) 2
      else if (rb) 3
      else if (lb) 4
    }.toSeq
    println("lt: " + res.count(_ == 1))
    println("rt: " + res.count(_ == 2))
    println("rb: " + res.count(_ == 3))
    println("lb: " + res.count(_ == 4))
    println("unknown: " + res.count(_ == 0))
  }
}