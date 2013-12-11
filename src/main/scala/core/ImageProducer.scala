package core

import java.awt.image.BufferedImage
import java.io.File

import scala.sys.process.stringSeqToProcess

import javax.imageio.ImageIO
import scalax.io.Resource

object ImageProducer {
  def rawToJpg(filename: String) = {
    val dirName = filename + "-jpg"
    assert(Seq("rm", "-rf", dirName).! == 0)
    assert(Seq("mkdir", dirName).! == 0)
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
          ImageIO.write(img, "jpg", new File(dirName + "/" + i + ".jpg"))
      }
  }
}