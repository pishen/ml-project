package core

import scala.Array.canBuildFrom

object FeatureExtractor {
  def apply(sample: Sample): Seq[Double] = {
    //TODO implement
    val subMxs = getSubMatrixs(getScaledMatrix(sample.matrix))
    littleProjectionLength(subMxs) ++ subMxs.map(_.map(_.sum).sum)
  }

  private def littleProjectionLength(subMatrixs: Seq[Seq[Seq[Double]]]) = {
    def twoNorm(v: Seq[Double]) = v.map(i => i * i).sum
    val ninety = subMatrixs.flatMap(m => {
      val yv = m.map(_.sum)
      val xv = m.reduce(_.zip(_).map(p => p._1 + p._2))
      Seq(twoNorm(yv), twoNorm(xv))
    })
    val fourtyFive = {
      val base = Seq.fill(subMatrixs.head.size, subMatrixs.head.size - 1)(0.0).zipWithIndex
      subMatrixs.flatMap(m => {
        val sw = base.map { case (zeros, i) => zeros.patch(i, m(i), 0) }
          .reduce(_.zip(_).map(p => p._1 + p._2))
        val se = base.map { case (zeros, i) => zeros.patch(zeros.size - i, m(i), 0) }
          .reduce(_.zip(_).map(p => p._1 + p._2))
        Seq(twoNorm(sw), twoNorm(se))
      })
    }
    ninety ++ fourtyFive
  }

  private def getSubMatrixs(matrix: Array[Array[Double]]) = {
    matrix.grouped(10).toSeq.map(rowGroup => {
      rowGroup.map(_.grouped(10).toSeq.map(arr => Seq(arr.toSeq)))
        .reduceLeft((l, r) => l.zip(r).map(p => p._1 ++ p._2))
    }).reduceLeft((l, r) => l ++ r)
  }

  private def getScaledMatrix(matrix: Array[Array[Double]]) = {
    val top = matrix.indexWhere(_.find(_ > 0.3).nonEmpty)
    val bottom = matrix.lastIndexWhere(_.find(_ > 0.3).nonEmpty)
    val left = matrix.head.indices.indexWhere(j => matrix.map(_(j)).find(_ > 0.3).nonEmpty)
    val right = matrix.head.indices.lastIndexWhere(j => matrix.map(_(j)).find(_ > 0.3).nonEmpty)
    if (top == -1) Array.fill(120, 120)(0.0)
    else {
      val height = bottom - top
      val width = right - left
      val edgeLength = height max width
      val baseI = (top + bottom) / 2 - edgeLength / 2
      val baseJ = (left + right) / 2 - edgeLength / 2
      Array.tabulate(120, 120)((i, j) => {
        try {
          matrix(baseI + ((i / 120.0) * edgeLength).toInt)(baseJ + ((j / 120.0) * edgeLength).toInt)
        } catch {
          case ex: Exception => 0.0
        }
      })
    }
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