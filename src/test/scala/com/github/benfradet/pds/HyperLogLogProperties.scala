package com.github.benfradet.pds

import org.scalacheck.Prop._
import org.scalacheck.{Gen, Properties}

class HyperLogLogProperties extends Properties("HyperLogLog") {
  def strsList(n: Int = 1000) = Gen.listOfN(n, Gen.alphaLowerChar.map(_.toString))

  property("compute the correct cardinality for a small error") =
    forAll(strsList()) { l: List[String] =>
      val hll = new HyperLogLog[String](0.01d)
      l.foreach(hll.add)
      hll.cardinality == l.distinct.size
    }
}
