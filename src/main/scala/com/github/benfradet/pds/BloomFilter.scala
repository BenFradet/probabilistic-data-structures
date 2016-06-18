package com.github.benfradet.pds

import scala.collection.mutable
import scala.util.hashing.MurmurHash3

/**
 * Intro: https://en.wikipedia.org/wiki/Bloom_filter
 * This implementation has been heavily inspired by Breeze's:
 * https://github.com/scalanlp/breeze/blob/master/math/src/main/scala/breeze/util/BloomFilter.scala
 */
class BloomFilter[T](m: Int, k: Int) {
  private val bits = mutable.BitSet(m)

  private def buckets(elem: T): Seq[Int] = {
    val hash1 = elem.##
    val hash2 = math.abs(MurmurHash3.mixLast(0, hash1))

    (0 to k).map { i =>
      val h = hash1 + i * hash2
      val nextHash = if (h < 0) ~h else h
      nextHash % k
    }
  }

  def apply(elem: T): Boolean = contains(elem)

  def contains(elem: T): Boolean =
    buckets(elem).forall(bits.contains)

  def add(elem: T): Unit =
    buckets(elem).foreach(bits.add)
}

object BloomFilter {
  def optimalSize(expectedNumItems: Long, falsePositiveRate: Double): (Int, Int) = {
    val n = expectedNumItems
    val p = falsePositiveRate
    import scala.math._
    val m = ceil(-n * log(p) / pow(log(2), 2))
    val k = round(m / n * log(2))
    (m.toInt, k.toInt)
  }

  def optimallySized[T](expectedNumItems: Long, falsePositiveRate: Double): BloomFilter[T] = {
    val (m, k) = optimalSize(expectedNumItems, falsePositiveRate)
    new BloomFilter(m, k)
  }
}
