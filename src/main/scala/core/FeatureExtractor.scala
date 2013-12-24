package core

import scala.Array.canBuildFrom

object FeatureExtractor {
  def apply(sample: Sample): Seq[Double] = {
    //TODO implement
    //getProjection(sample.matrix) ++ getLostCorners(sample.matrix)
    quantize(sample.matrix)
  }
  
  private def quantize(matrix: Array[Array[Double]]): Seq[Double] = {
    val ten = matrix.slice(1, 121).grouped(10).toSeq.map(rowGroup => {
      rowGroup.map(_.slice(2, 102).grouped(10).toSeq.map(_.sum)).reduce(_.zip(_).map(p => p._1 + p._2))
    }).reduceLeft((l, r) => l ++ r)
    val five = matrix.slice(31, 91).grouped(5).toSeq.map(rowGroup => {
      rowGroup.map(_.slice(27, 77).grouped(5).toSeq.map(_.sum)).reduce(_.zip(_).map(p => p._1 + p._2))
    }).reduceLeft((l, r) => l ++ r)
    ten ++ five
  }

  private def getProjection(matrix: Array[Array[Double]]) = {
    val yVector = matrix.map(row => row.sum)
    val xVector = for (j <- matrix.head.indices) yield {
      (for (i <- matrix.indices) yield matrix(i)(j)).sum
    }
    xVector ++ yVector
  }

  private def getLostCorners(matrix: Array[Array[Double]]) = {
    def isLost(sub: Seq[Double]) = if (sub.find(_ > 0).isEmpty) 1.0 else 0.0

    val lt = for (i <- 0 to 60; j <- 0 to 51) yield matrix(i)(j)
    val rt = for (i <- 0 to 60; j <- 53 to 104) yield matrix(i)(j)
    val rb = for (i <- 61 to 121; j <- 53 to 104) yield matrix(i)(j)
    val lb = for (i <- 61 to 121; j <- 0 to 51) yield matrix(i)(j)
    Seq(lt, rt, rb, lb).map(isLost _)
  }

}