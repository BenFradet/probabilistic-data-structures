package com.github.benfradet.pds

import scala.math._
import scala.util.hashing.MurmurHash3

/**
 * Intro: https://en.wikipedia.org/wiki/HyperLogLog
 * This implementation has been inspired by:
 * https://github.com/addthis/stream-lib/blob/master/src/main/java/com/clearspring/analytics/stream/cardinality/HyperLogLog.java
 * Reference paper: http://algo.inria.fr/flajolet/Publications/FlFuGaMe07.pdf
 */

/** @param numBits number of bits to use */
class HyperLogLog[T](val numBits: Int) {
  /** @param rsd relative standard deviation */
  def this(rsd: Double) = this((log((1.106 / rsd) * (1.106 / rsd)) / log(2)).toInt)

  /** relative standard deviation */
  val rsd = 1.106 / sqrt(exp(numBits * log(2)))

  private val bitSet = new BitSet(1 << numBits)

  /** add an element to the hll */
  def add(elem: T): Unit = {
    val hash = MurmurHash3.mixLast(0, elem.##)
    // j is the binary address determined by the first b bits of hash between 0 and and 2^b
    val j = hash >>> (Integer.SIZE - numBits)
    val r = Integer.numberOfLeadingZeros((hash - numBits) | (1 << (numBits - 1)) + 1) + 1
    bitSet.updateIfGreater(j, r)
  }

  /** Number of distinct element in the hll */
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

private class BitSet(count: Int) {
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
