package core

import scala.Array.canBuildFrom

object FeatureExtractor {
  def apply(sample: Sample) = {
    val cornerFilled = LostCornerHandler.tryFillCorner(sample)
    val subMxs = getSubMatrixs(getScaledMatrix(cornerFilled))
    littleProjectionLength(subMxs) ++ subMxs.map(_.map(_.sum).sum)
  }

  private def littleProjectionLength(subMatrixs: Seq[Seq[Seq[Double]]]) = {
    def twoNorm(v: Seq[Double]) = v.map(i => i * i).sum
    subMatrixs.flatMap(m => {
      val yv = m.map(_.sum)
      val xv = m.reduce(_.zip(_).map(p => p._1 + p._2))
      Seq(twoNorm(yv), twoNorm(xv))
    })
  }

  private def getSubMatrixs(matrix: Array[Array[Double]]) = {
    matrix.grouped(10).toSeq.map(rowGroup => {
      rowGroup.map(_.grouped(10).toSeq.map(arr => Seq(arr.toSeq)))
        .reduceLeft((l, r) => l.zip(r).map(p => p._1 ++ p._2))
    }).reduceLeft((l, r) => l ++ r)
  }

  private def isEmpty(matrix: Array[Array[Double]]) = {
    matrix.flatMap(r => r).find(_ > 0.3).isEmpty
  }

  /*private def threshold(matrix: Array[Array[Double]]) = {
    matrix.map(_.map(c => if (c > 0.3) 1.0 else 0.0))
  }*/

  private def getScaledMatrix(sample: Sample) = {
    val top = sample.top
    val bottom = sample.bottom
    val left = sample.left
    val right = sample.right
    assert(top != -1 && left != -1)
    if (top == -1) Array.fill(120, 120)(0.0)
    else {
      val height = bottom - top
      val width = right - left
      val edgeLength = height max width
      val baseI = (top + bottom) / 2 - edgeLength / 2
      val baseJ = (left + right) / 2 - edgeLength / 2
      Array.tabulate(120, 120)((i, j) => {
        try {
          sample.matrix(baseI + ((i / 120.0) * edgeLength).toInt)(baseJ + ((j / 120.0) * edgeLength).toInt)
        } catch {
          case ex: Exception => 0.0
        }
      })
    }
  }

}