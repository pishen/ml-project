package core

import java.io.File
import java.io.FileWriter

import scala.math.pow
import scala.sys.process.stringSeqToProcess

import scalax.io.Resource

object Main {

  def main(args: Array[String]): Unit = {

    val scaledFiles = Seq("PCA_1000_train.libsvm.s", "PCA_1000_test.libsvm.s")

    println("polyCV")
    val (params, accu) = polyCV(scaledFiles.head)
    println("best CV: " + accu)

    println("polyTrain")
    val model = polyTrain(scaledFiles.head, params)

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
    assert((Seq("./svm-scale", "-l", "0", "-s", "range", filenames.head) #> new File(filenames.head + ".s")).! == 0)
    filenames.tail.foreach(name => {
      assert((Seq("./svm-scale", "-l", "0", "-r", "range", name) #> new File(name + ".s")).! == 0)
    })
    filenames.map(_ + ".s")
  }

  case class Params(gamma: Double, degree: Int, cost: Double)
  def polyCV(trainName: String) = {
    def svmCV(params: Params) = {
      val res = Seq(
        "./svm-train",
        "-t", "1",
        "-d", params.degree.toString,
        "-g", params.gamma.toString,
        "-r", "1",
        "-c", params.cost.toString,
        "-v", "5",
        "-m", "1000",
        trainName).!!
      res.split("\n").last.split(" ").last.init.toDouble / 100
    }
    val gamma = 0.001
    val degrees = Seq(2, 3, 4, 5)
    val costs = Seq(2, 4, 6, 8).map(pow(2, _))

    val ress = for (degree <- degrees; cost <- costs) yield {
      val params = Params(gamma, degree, cost)
      println("gamma: " + gamma + " degree: " + degree + " cost: " + cost)
      (params, svmCV(params))
    }
    val line1 = costs.map("c=" + _).mkString(" ")
    val lines = degrees.map(d => {
      "d=" + d + " " + ress.filter(_._1.degree == d).sortBy(_._1.cost).map(_._2).mkString(" ")
    })
    Resource.fromWriter(new FileWriter(trainName + ".poly")).writeStrings(line1 +: lines, "\n")

    ress.maxBy(_._2)
  }

  def polyTrain(trainName: String, params: Params) = {
    Seq(
      "./svm-train",
      "-t", "1",
      "-d", params.degree.toString,
      "-g", params.gamma.toString,
      "-r", "1",
      "-c", params.cost.toString,
      "-m", "1000",
      trainName, trainName + ".m").!!
    trainName + ".m"
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
    if (gamma != null) {
      assert(Seq("./svm-train", "-c", cost, "-g", gamma, "-q", trainName, trainName + ".m").! == 0)
    } else {
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