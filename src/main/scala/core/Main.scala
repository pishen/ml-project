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
    /*println("subset.py")
    assert(Seq("./subset.py", "train-all", "1000", "validate", "train").! == 0)*/

    val rawFiles = Seq("PCA_1000_train.libsvm", "PCA_1000_test.libsvm")
    //println("extract features")
    //val featureFiles = extractFeature(rawFiles)
    println("svm-scale")
    val scaledFiles = scale(rawFiles)
    println("grid.py")
    val (cost, gamma) = grid(scaledFiles.head)
    println("svm-train")
    val model = svmTrain(scaledFiles.head, cost, gamma)
    println("svm-predict")
    svmPredict(scaledFiles.last, model)
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

  def scale(filenames: Seq[String]) = {
    //use the first file as range base
    assert((Seq("./svm-scale", "-s", "range", filenames.head) #> new File(filenames.head + ".s")).! == 0)
    filenames.tail.foreach(name => {
      assert((Seq("./svm-scale", "-r", "range", name) #> new File(name + ".s")).! == 0)
    })
    filenames.map(_ + ".s")
  }

  def grid(trainName: String) = {
    //TODO use process logger?
    val res = Seq(
      "./grid.py",
      "-log2c", "0,6,2",
      "-log2g", "-6,0,2",
      "-svmtrain", "./svm-train",
      "-m", "1000",
      trainName).!!
    println(res)
    
    //get the best cost
    //res.split("\n").last.split(" ").head
    
    //get the best cost & gamma
    val tokens = res.split("\n").last.split(" ")
    (tokens(0), tokens(1))
  }
  
  def svmTrain(trainName: String, cost: String, gamma: String = null) = {
    if(gamma != null){
      assert(Seq("./svm-train", "-c", cost, "-g", gamma, "-q", trainName, trainName + ".m").! == 0)
    }else{
      assert(Seq("./svm-train", "-c", cost, "-q", trainName, trainName + ".m").! == 0)
    }
    trainName + ".m"
  }
  
  def svmPredict(testName: String, modelName: String) = {
    assert(Seq("./svm-predict", testName, modelName, "predict").! == 0)
    "predict"
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
    label + " " + features.zipWithIndex.filter(_._1 != 0.0).map(p => (p._2 + 1) + ":" + p._1).mkString(" ")
  }

}