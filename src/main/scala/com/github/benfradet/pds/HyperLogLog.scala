package com.github.benfradet.pds

import scala.math._
import scala.util.hashing.MurmurHash3

/**
 * From: https://en.wikipedia.org/wiki/HyperLogLog
 *
 * HyperLogLog counts the number of distinct elements in a stream of data.
 *
 * Calculating the exact cardinality of a multiset requires an amount of memory proportional to
 * the cardinality, which is impractical for very large datasets. Probabilistic cardinality
 * estimators, such as the HyperLogLog algorithm, use significantly less memory than this, at the
 * cost of obtaining an approximation of the cardinality. The HyperLogLog algorithm is able to
 * estimate cardinalities of ''>10^9'' with a typical error rate of 2%, using 1.5kB of memory.
 * HyperLogLog is an extension of the earlier LogLog algorithm.
 *
 * The basis of the HyperLogLog algorithm is the observation that the cardinality of a stream of
 * uniformly random numbers can be estimated by calculating the maximum number of leading zeros
 * in the binary representation of each number in the stream. If the maximum number of leading
 * zeros observed is ''n'', an estimate for the number of distinct elements in the set is ''2^n^''.
 *
 * In the HyperLogLog algorithm a hash function is applied to each element in the original
 * multiset, to obtain a multiset of uniformly distributed random numbers with the same
 * cardinality as the original multiset. The cardinality of this randomly distributed set can be
 * estimated using the algorithm above.
 *
 * The simple estimate of cardinality obtained using the algorithm above has the disadvantage of
 * a large variance. In the HyperLogLog algorithm, the variance is minimised by splitting the
 * multiset into numerous subsets, calculating the number of leading zeros in the numbers in each
 * of these subsets, and using a harmonic mean to combine these estimates for each subset into an
 * estimate of the cardinality of the whole set.
 *
 * This implementation has been inspired by:
 * https://github.com/addthis/stream-lib/blob/master/src/main/java/com/clearspring/analytics/stream/cardinality/HyperLogLog.java
 *
 * Reference paper: http://algo.inria.fr/flajolet/Publications/FlFuGaMe07.pdf
 */

/** @param numBits number of bits to use */
class HyperLogLog[T](val numBits: Int) {
  /** @param rsd relative standard deviation */
  def this(rsd: Double) = this((log((1.106 / rsd) * (1.106 / rsd)) / log(2)).toInt)

  val rsd = 1.106 / sqrt(exp(numBits * log(2)))
  val accuracy = 1.04 / sqrt(pow(2, numBits.toDouble))

  private val bitSet = new BitSet(1 << numBits)

  def add(elem: T): Unit = {
    val hash = MurmurHash3.mixLast(0, elem.##)
    // j is the binary address determined by the first b bits of hash between 0 and and 2^b
    val j = hash >>> (Integer.SIZE - numBits)
    val r = Integer.numberOfLeadingZeros((hash - numBits) | (1 << (numBits - 1)) + 1) + 1
    bitSet.updateIfGreater(j, r)
  }

  def cardinality: Long = {
    val count = 1 << numBits
    val values = (0 until count).map(bitSet.get)
    val sum = values.foldLeft(0d)((s, v) => s + 1.0 / (1 << v))
    val zeros = values.count(_ == 0).toDouble
    val estimate = alphaMM(numBits, 1 << numBits) * (1 / sum)
    import scala.math._
    if (estimate <= (5.0 / 2.0) * count) round(count * log(count / zeros))
    else round(estimate)
  }

  private val alphaMM = (p: Int, m: Int) => m * m * p match {
    case 4 => 0.673
    case 5 => 0.697
    case 6 => 0.709
    case _ => 0.7213 / (1 + 1.079 / m)
  }
}

class BitSet(count: Int) {
  private val LOG2_BITS_PER_WORD = 6
  private val REGISTER_SIZE = 5
  private val array = new Array[Int](sizeForCount(count))

  def updateIfGreater(pos: Int, value: Int): Unit = {
    val bucket = getBucket(pos)
    val shift = getShift(pos, bucket)
    val mask = 0x1f << shift

    val curVal: Long = (array(bucket) & mask).toLong
    val newVal: Long = (value << shift).toLong
    if (curVal < newVal) array(bucket) = ((array(bucket) & ~mask) | newVal).toInt
  }

  def get(pos: Int): Int = {
    val bucket = getBucket(pos)
    val shift = getShift(pos, bucket)
    (array(bucket) & (0x1f << shift)) >>> shift
  }

  private def sizeForCount(count: Int): Int = {
    val bits = count / LOG2_BITS_PER_WORD
    if (bits == 0) 1
    else if (bits % Integer.SIZE == 0) bits
    else bits + 1
  }

  private def getBucket(pos: Int): Int = pos / LOG2_BITS_PER_WORD
  private def getShift(pos: Int, bucket: Int): Int =
    REGISTER_SIZE * (pos - (bucket * LOG2_BITS_PER_WORD))
}
