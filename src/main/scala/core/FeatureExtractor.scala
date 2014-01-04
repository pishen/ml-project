package core

import scala.Array.canBuildFrom

object FeatureExtractor {
  val threshold = 0.5

  def apply(sample: Sample) = {
    val scaledMatrix = getScaledMatrix(sample)
    //val slicedMatrix = sample.matrix.slice(1, 121).map(_.slice(2, 102))
    val subMatrices15 = getSubMatrices(scaledMatrix, 15)
    val subMatrices10 = getSubMatrices(scaledMatrix, 10)

    subMatrices15.flatMap(crossCount) ++
      subMatrices10.map(_.map(_.sum).sum) ++
      sideDepth(scaledMatrix, 3)
  }

  private def projectionWeight(matrix: Seq[Seq[Double]]) = {
    def twoNorm(v: Seq[Double]) = v.map(i => i * i).sum

    val yv = matrix.map(_.sum)
    val xv = matrix.reduce(_.zip(_).map(p => p._1 + p._2))
    Seq(twoNorm(yv), twoNorm(xv))
  }

  private def sideDepth(matrix: Seq[Seq[Double]], size: Int) = {
    def groupAvg(g: Seq[Int]) = {
      val fg = g.filter(_ >= 0)
      if (fg.nonEmpty) fg.sum / fg.size.toDouble else -10
    }
    val left = matrix.map(_.indexWhere(_ > threshold))
      .grouped(size).toSeq.map(groupAvg)
    val right = matrix.map(_.reverse.indexWhere(_ > threshold))
      .grouped(size).toSeq.map(groupAvg)
    val top = matrix.head.indices.map(j => matrix.map(_(j))).map(_.indexWhere(_ > threshold))
      .grouped(size).toSeq.map(groupAvg)
    val bottom = matrix.head.indices.map(j => matrix.map(_(j))).map(_.reverse.indexWhere(_ > threshold))
      .grouped(size).toSeq.map(groupAvg)
    left ++ right ++ top ++ bottom
  }

  private def borderDirection(matrix: Seq[Seq[Double]]) = {
    def count(seq: Seq[Double]) = {
      seq.map(_ > threshold).sliding(2).count(seq => seq.head != seq.last)
    }
    val top = matrix.head.init
    val right = matrix.init.map(_.last)
    val bottom = matrix.last.tail.reverse
    val left = matrix.tail.map(_.head).reverse
    Seq(top, right, bottom, left).map(count)
  }

  private def borderCross(matrix: Seq[Seq[Double]]) = {
    val border = matrix.head.init ++
      matrix.init.map(_.last) ++
      matrix.last.tail.reverse ++
      matrix.tail.map(_.head).reverse
    val count = border.map(_ > threshold).sliding(2).count(seq => seq.head != seq.last)
    if (count > 4) 1 else 0
  }
  
  private def largeCrossVector(matrix: Seq[Seq[Double]], size: Int) = {
    def countChanges(seq: Seq[Double]) = {
      seq.map(_ > threshold).sliding(2).count(seq => seq.head != seq.last)
    }
    val xChanges = matrix.indices.filter(_ % size == 0).map(i => matrix(i)).map(countChanges)
    val ySeqs = matrix.head.indices.map(j => matrix.map(_(j)))
    val yChanges = ySeqs.indices.filter(_ % size == 0).map(j => ySeqs(j)).map(countChanges)
    xChanges.map(_.toDouble) ++ yChanges.map(_.toDouble)
  }

  private def crossCount(matrix: Seq[Seq[Double]]) = {
    def countChanges(seq: Seq[Double]) = {
      seq.map(_ > threshold).sliding(2).count(seq => seq.head != seq.last)
    }
    val xChanges = matrix.map(countChanges)
    val yChanges = matrix.head.indices.map(j => matrix.map(_(j))).map(countChanges)
    Seq(xChanges.sum / xChanges.size.toDouble, yChanges.sum / yChanges.size.toDouble)
  }

  private def getSubMatrices(matrix: Seq[Seq[Double]], size: Int) = {
    matrix.grouped(size).toSeq.flatMap(rowGroup => {
      val gg = rowGroup.map(_.grouped(size).toSeq)
      gg.head.indices.map(j => gg.map(_(j)))
    })
  }

  private def getScaledMatrix(sample: Sample) = {
    val top = sample.top
    val bottom = sample.bottom
    val left = sample.left
    val right = sample.right
    if (top == -1) Seq.fill(120, 120)(0.0)
    else {
      val height = bottom - top
      val width = right - left
      val edgeLength = height max width
      val baseI = (top + bottom) / 2 - edgeLength / 2
      val baseJ = (left + right) / 2 - edgeLength / 2
      Seq.tabulate(120, 120)((i, j) => {
        try {
          sample.matrix(baseI + ((i / 120.0) * edgeLength).toInt)(baseJ + ((j / 120.0) * edgeLength).toInt)
        } catch {
          case ex: Exception => 0.0
        }
      })
    }
  }

}