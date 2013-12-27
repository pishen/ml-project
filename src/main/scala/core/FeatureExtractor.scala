package core

import scala.Array.canBuildFrom

object FeatureExtractor {
  def apply(sample: Sample): Seq[Double] = {
    //TODO implement
    val subMxs = getSubMatrixs(sample.matrix)
    littleProjectionLength(subMxs) ++ subMxs.map(_.map(_.sum).sum)
    //divide4Parts(subMxs)
  }
  
  private def divide4Parts(subMatrixs: Seq[Seq[Seq[Double]]]) = {
    subMatrixs.map(m => {
      val yv = m.map(_.sum)
      val xv = m.reduce(_.zip(_).map(p => p._1 + p._2))
      yv ++ xv
    }).reduceLeft((l, r) => l ++ r)
  }
  
  private def littleProjectionLength(subMatrixs: Seq[Seq[Seq[Double]]]) = {
    def twoNorm(v: Seq[Double]) = v.map(i => i * i).sum
    subMatrixs.map(m => {
      val yv = m.map(_.sum)
      val xv = m.reduce(_.zip(_).map(p => p._1 + p._2))
      Seq(twoNorm(yv), twoNorm(xv))
    }).reduceLeft((l, r) => l ++ r)
  }

  private def getSubMatrixs(matrix: Array[Array[Double]]) = {
    matrix.slice(1, 121).grouped(20).toSeq.map(rowGroup => {
      rowGroup.map(_.slice(2, 102).grouped(20).toSeq.map(arr => Seq(arr.toSeq)))
        .reduceLeft((l, r) => l.zip(r).map(p => p._1 ++ p._2))
    }).reduceLeft((l, r) => l ++ r)
  }
  
  private def getScaledMatrix(matrix: Array[Array[Double]]) = {
    
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