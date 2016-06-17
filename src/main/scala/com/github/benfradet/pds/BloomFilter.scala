package com.github.benfradet.pds

import scala.collection.mutable
import scala.util.hashing.MurmurHash3

/**
 * From: https://en.wikipedia.org/wiki/Bloom_filter
 *
 * A Bloom filter tests if an element is a member of a set.
 *
 * False positives are possible (returns that an element is in the set while it's not) but false
 * negatives are not possible (does not return that an element is in the set if it's not).
 * Elements can be added to the set but not removed.
 * The more elements are added to the set, the larger the probability of false positives.
 *
 * An empty Bloom filter is a bit array of ''m'', all set to 0 and ''k'' different hash functions
 * each of which maps some set element to one of the ''m'' array positions with a uniform random
 * distribution.
 * Typically, ''k'' is a much smaller than ''m'', which is proportional to the number of elements to
 * be added, the precise choice of ''k'' and the constant of proportionality of ''m'' are
 * determined by the intended false positive rate of the filter.
 *
 * To ''add'' an element, feed it to each of the ''k'' hash functions to get ''k'' array positions
 * which are consequently set to 1 (O(k) complexity).
 *
 * To ''query'' for an element, feed it to each of the ''k'' hash functions to get ''k'' array
 * positions (O(k) complexity). If any of the bits is 0, the element is definitely not in the set.
 * If all bits are 1, then either the element is in the set, or the bits have by chance been set to
 * 1 during the insertion of other elements, resulting in a false positive.
 *
 * The probability of false positives is given by pow((1 - exp(-kn/m)), k)
 *
 * Given ''m'' and ''n'', the optimal ''k'' is m/n * ln 2
 *
 * Given the wanted percentage of false positives ''p'' and ''n'', ''m'' is
 * - (n ln p) / pow(ln 2, 2)
 *
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
