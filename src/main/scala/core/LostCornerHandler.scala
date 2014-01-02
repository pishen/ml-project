package core

import scala.math.abs

import scalax.io.Resource

object LostCornerHandler {

  def detectAndSave(s: Sample, raw: String) = {
    //filename: label-lostCorner
    if (s.lost != 4) Resource.fromFile("corner/" + s.label + "-" + s.lost).write(raw + "\n")
  }

  def tryFillCorner(s: Sample) = {
    if (s.lost == 4) s
    else {
      val rescues = (0 to 3).filter(_ != s.lost)
        .map(l => "corner/" + s.label + "-" + l)
        .flatMap(Resource.fromFile(_).lines().toSeq)
        .map(Main.decode(_))
      rescues.map(combineAndDistance(s, _)).minBy(_._2)._1
    }
  }

  def combineAndDistance(s: Sample, rescue: Sample) = {
    if (s.lost == 0) {
      val newS = Sample(s.label, Array.tabulate(122, 105)((i, j) => {
        if (i <= 60 && j <= 51) rescue.matrix(i)(j) else s.matrix(i)(j)
      }))
      (newS, abs(s.left - newS.left) + abs(s.top - newS.top))
    } else if (s.lost == 1) {
      val newS = Sample(s.label, Array.tabulate(122, 105)((i, j) => {
        if (i <= 60 && j >= 53) rescue.matrix(i)(j) else s.matrix(i)(j)
      }))
      (newS, abs(s.top - newS.top) + abs(s.right - newS.right))
    } else if (s.lost == 2) {
      val newS = Sample(s.label, Array.tabulate(122, 105)((i, j) => {
        if (i >= 61 && j <= 51) rescue.matrix(i)(j) else s.matrix(i)(j)
      }))
      (newS, abs(s.left - newS.left) + abs(s.bottom - newS.bottom))
    } else {
      val newS = Sample(s.label, Array.tabulate(122, 105)((i, j) => {
        if (i >= 61 && j >= 53) rescue.matrix(i)(j) else s.matrix(i)(j)
      }))
      (newS, abs(s.right - newS.right) + abs(s.bottom - newS.bottom))
    }
  }

}