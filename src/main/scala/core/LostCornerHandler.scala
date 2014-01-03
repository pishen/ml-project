package core

import scala.math.abs

import scalax.io.Resource

object LostCornerHandler {

  def detectAndSave(s: Sample, raw: String) = {
    //filename: label-lostCorner
    if (s.lost != 4) Resource.fromFile("corner/" + s.label + "-" + s.lost).write(raw + "\n")
  }

  

}