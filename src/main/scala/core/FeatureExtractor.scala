package core

import scala.Array.canBuildFrom

object FeatureExtractor {
  def apply(sample: Sample): Seq[Double] = {
    //TODO implement
    getProjection(sample.matrix)
  }
  
  private def getProjection(matrix: Array[Array[Double]]) = {
    val yVector = matrix.map(row => row.sum)
    val xVector = for (j <- matrix.head.indices) yield {
      (for (i <- matrix.indices) yield matrix(i)(j)).sum
    }
    xVector ++ yVector
  }
  
  /*val input = Resource.fromFile(testName)
    val output = Resource.fromOutputStream(new FileOutputStream("output"))
    
    val newData = input.lines().map(line => (line.split(" ").head, getProjection(getMatrix(line)))).map{
      case (label, projection) => {
        label + " " + projection.zipWithIndex.filter(p => p._1 > 0.0).map(p => p._2 + 1 + ":" + p._1).mkString(" ")
      }
    }
    output.writeStrings(newData, "\n")*/
}