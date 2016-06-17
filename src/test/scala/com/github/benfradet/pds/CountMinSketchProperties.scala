package com.github.benfradet.pds

import org.scalacheck.Prop._
import org.scalacheck.{Gen, Properties}

class CountMinSketchProperties extends Properties("CountMinSketch") {
  def strsList(n: Int = 1000) = Gen.listOfN(n, Gen.alphaLowerChar.map(_.toString))

  property("has at least the correct occurrences for each word") =
    forAll(strsList()) { (l: List[String]) =>
      val eps = 0.005
      val delta = 1E-8
      val cms = new CountMinSketch[String](eps, delta)
      l.foreach(cms.add(_))
      val wc = wordCount(l)
      wc.forall{ case (w, c) =>
        val cmsValue = cms(w)
        // delta chance that the upper bound fails
        c <= cmsValue && cmsValue <= c + eps * l.size
      }
    }

  def wordCount(list: List[String]): Map[String, Int] =
    list.foldLeft(Map.empty[String, Int]) { (acc, w) =>
      acc + (w -> (acc.getOrElse(w, 0) + 1))
    }
}
