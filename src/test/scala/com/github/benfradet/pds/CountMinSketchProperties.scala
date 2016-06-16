package com.github.benfradet.pds

import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._
import org.scalacheck.{Gen, Properties}

class CountMinSketchProperties extends Properties("CountMinSketch") {
  def strsList(n: Int = 10000) = Gen.listOfN(n, arbitrary[String])

  property("has at least the correct occurrences for each word") =
    forAll(strsList()) { (l: List[String]) =>
      val cms = new CountMinSketch[String](0.005, 1E-8)
      l.foreach(cms.add(_))
      val wc = wordCount(l)
      wc.forall{ case (w, c) => c <= cms(w) }
    }

  def wordCount(list: List[String]): Map[String, Int] =
    list.foldLeft(Map.empty[String, Int]) { (acc, w) =>
      acc + (w -> (acc.getOrElse(w, 0) + 1))
    }
}
