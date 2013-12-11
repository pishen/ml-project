package core

import java.awt.image.BufferedImage
import java.io.File
import java.io.FileOutputStream

import scala.Array.canBuildFrom
import scala.sys.process.stringSeqToProcess

import javax.imageio.ImageIO
import scalax.io.Resource

object Main {

  def main(args: Array[String]): Unit = {
    val rawTrainName = "ml2013final_train.dat"
    val testName = "ml2013final_test1.nolabel.dat"
    
    
      
    val input = Resource.fromFile(testName)
    val output = Resource.fromOutputStream(new FileOutputStream("output"))
    
    val newData = input.lines().map(line => (line.split(" ").head, getProjection(getMatrix(line)))).map{
      case (label, projection) => {
        label + " " + projection.zipWithIndex.filter(p => p._1 > 0.0).map(p => p._2 + 1 + ":" + p._1).mkString(" ")
      }
    }
    output.writeStrings(newData, "\n")
  }

  def getProjection(matrix: Array[Array[Double]]) = {
    val yVector = matrix.map(row => row.sum)
    val xVector = for (j <- 0 until 105) yield {
      (for (i <- 0 until 122) yield matrix(i)(j)).sum
    }
    xVector.toSeq ++ yVector.toSeq
  }
  
  

  def getMatrix(line: String) = {
    val matrix = Array.ofDim[Double](122, 105)
    line.split(" ").tail.foreach { str =>
      val lr = str.split(":")
      val index = lr.head.toInt - 1
      val value = lr.last.toDouble
      matrix(index / 105)(index % 105) = value
    }
    matrix
  }
  
}