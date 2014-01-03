package core

case class Sample(label: Int, matrix: Array[Array[Double]]) {
  lazy val top =
    matrix.indexWhere(_.find(_ > FeatureExtractor.threshold).nonEmpty)
  lazy val bottom =
    matrix.lastIndexWhere(_.find(_ > FeatureExtractor.threshold).nonEmpty)
  lazy val left =
    matrix.head.indices.indexWhere(j => matrix.map(_(j)).find(_ > FeatureExtractor.threshold).nonEmpty)
  lazy val right =
    matrix.head.indices.lastIndexWhere(j => matrix.map(_(j)).find(_ > FeatureExtractor.threshold).nonEmpty)

  lazy val lost = {
    //lt:0 rt:1 lb:2 rb:3 unknown:4
    val lt = for (i <- 0 to 60; j <- 0 to 51) yield matrix(i)(j)
    val rt = for (i <- 0 to 60; j <- 53 to 104) yield matrix(i)(j)
    val lb = for (i <- 61 to 121; j <- 0 to 51) yield matrix(i)(j)
    val rb = for (i <- 61 to 121; j <- 53 to 104) yield matrix(i)(j)

    def isLost(seq: Seq[Double]) = seq.find(_ > 0.0).isEmpty
    val res = Seq(lt, rt, lb, rb).map(isLost _)
    if (res.count(lost => lost) != 1) 4 else res.indexWhere(lost => lost)
  }

  def getCorner(i: Int) = {
    if (i == 0) {
      matrix.take(61).map(_.take(52))
    } else if (i == 1) {
      matrix.take(61).map(_.takeRight(52))
    } else if (i == 2) {
      matrix.takeRight(61).map(_.take(52))
    } else {
      matrix.takeRight(61).map(_.takeRight(52))
    }
  }
}