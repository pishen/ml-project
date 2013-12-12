package core

import scala.Array.canBuildFrom

object FeatureExtractor {
  def apply(sample: Sample): Seq[Double] = {
    //TODO implement
    //getProjection(sample.matrix) ++ getLostCorners(sample.matrix)
    getCoarsePixels(sample.matrix) ++ getProjection(sample.matrix)
  }

  private def getCoarsePixels(matrix: Array[Array[Double]]) = {
    matrix.grouped(10).toSeq.init.map(sub => {
      sub.map(_.grouped(8).toSeq.init.map(_.sum)).reduce(_.zip(_).map(p => p._1 + p._2))
    }).reduceLeft((l, r) => l ++ r)
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