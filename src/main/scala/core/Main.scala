package core

import java.io.File
import java.io.FileWriter

import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION
import scala.sys.process.stringSeqToProcess

import scalax.io.Resource

object Main {

  def main(args: Array[String]): Unit = {
    //subset
    assert(Seq("./subset.py", "train-all", "1000", "validate", "train").! == 0)

    val rawFiles = Seq("train", "validate", "test1")
    val featureFiles = extractFeature(rawFiles)
    val scaledFiles = scale(featureFiles)
    
  }

  def extractFeature(filenames: Seq[String]) = {
    filenames.foreach(filename => {
      val input = Resource.fromFile(filename).lines()
      Resource.fromWriter(new FileWriter(filename + ".f")).writeStrings({
        input.map(decode _).map(s => encode(s.label, FeatureExtractor(s)))
      }, "\n")
    })
    filenames.map(_ + ".f")
  }

  //use the first file as range base
  def scale(filenames: Seq[String]) = {
    assert((Seq("./svm-scale", "-l", "0", "-s", "range", filenames.head) #> new File(filenames.head + ".s")).! == 0)
    filenames.tail.foreach(name => {
      assert((Seq("./svm-scale", "-l", "0", "-r", "range", name) #> new File(name + ".s")).! == 0)
    })
    filenames.map(_ + ".s")
  }

  //data parser
  def decode(line: String) = {
    val matrix = Array.ofDim[Double](122, 105)
    val tokens = line.split(" ")
    tokens.tail.foreach { str =>
      val lr = str.split(":")
      val index = lr.head.toInt - 1
      val value = lr.last.toDouble
      matrix(index / 105)(index % 105) = value
    }
    Sample(tokens.head.toInt, matrix)
  }

  def encode(label: Int, features: Seq[Double]) = {
    label + " " + features.zipWithIndex.filter(_._1 > 0.0).map(p => (p._2 + 1) + ":" + p._1).mkString(" ")
  }

}