package core

import scalax.io.Resource
import scala.io.Source
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.File

object Main {

  def main(args: Array[String]): Unit = {
    val trainName = "ml2013final_train.dat"
    val testName = "ml2013final_test1.nolabel.dat"
  }

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

  def rawToJpg(filename: String) = {
    Resource.fromFile(filename).lines()
      .zipWithIndex.foreach {
        case (line, i) =>
          println(i)
          val img = new BufferedImage(105, 122, BufferedImage.TYPE_INT_RGB)
          for (x <- 0 until 105; y <- 0 until 122) {
            img.setRGB(x, y, (255 << 16) | (255 << 8) | 255)
          }
          line.split(" ").tail.foreach { str =>
            val lr = str.split(":")
            val index = lr.head.toInt - 1
            val value = (255 * (1 - lr.last.toDouble)).toInt
            val rgb = (value << 16) | (value << 8) | value
            img.setRGB(index % 105, index / 105, rgb)
          }
          ImageIO.write(img, "jpg", new File("ml-test-jpg/" + i + ".jpg"))
      }
  }
}