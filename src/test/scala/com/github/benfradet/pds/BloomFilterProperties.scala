package com.github.benfradet.pds

import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._
import org.scalacheck.{Gen, Properties}

class BloomFilterProperties extends Properties("BloomFilter") {
  val smallInts = Gen.choose(1, 1000)
  def strsList(n: Int = 10) = Gen.listOfN(n, arbitrary[String])
  def strsSet(n: Int = 10) = strsList(n).map(_.toSet).suchThat(_.nonEmpty)

  property("contain added values") =
    forAll(strsList(), smallInts, smallInts) {
      (strings: List[String], numBuckets: Int, numHashes: Int) =>
        val bf = new BloomFilter[String](numBuckets, numHashes)
        strings.foreach(bf.add)
        strings.forall(bf.contains)
    }
  property("not contain not added values (most of the time)") =
    forAll(strsSet(), strsSet(), smallInts, smallInts) {
      (strs1: Set[String], strs2: Set[String], numBuckets: Int, numHashes: Int) =>
        val bf = new BloomFilter[String](numBuckets, numHashes)
        strs1.foreach(bf.add)
        val diff = strs2 -- strs1
        val numBad = diff.count(bf.contains)
        val threshold = diff.size
        numBad <= threshold
    }
}
