package com.github.benfradet.pds

import scala.math._
import scala.util.hashing.MurmurHash3

/**
 * Intro: https://en.wikipedia.org/wiki/Count%E2%80%93min_sketch
 * This implementation has been by inspired Algebird's:
 * https://github.com/twitter/algebird/blob/develop/algebird-core/src/main/scala/com/twitter/algebird/CountMinSketch.scala
 * Reference paper: http://dimacs.rutgers.edu/~graham/pubs/papers/cmencyc.pdf
 */

/**
 * @param w width of the cms
 * @param d depth of the cms, number of hash functions used
 */
class CountMinSketch[T](w: Int, d: Int) {
  /**
   * estimates will be upper-bounded by a_i + epsilon * sum(a_j) with probability 1 - delta
   * @param eps epsilon
   * @param delta delta
   */
  def this(eps: Double, delta: Double) =
    this(ceil(exp(1) / eps).toInt, ceil(-log(1 - delta) / log(2)).toInt)

  private val counts = Array.ofDim[Long](d, w)

  /** retrieve the estimated count for a particular element */
  def apply(elem: T): Long = getCount(elem)

  /** retrieve the estimated count for a particular element */
  def getCount(elem: T): Long =
    buckets(elem)
      .map { case (j, k) => counts(j)(k) }
      .min

  /** add an element to the cms */
  def add(elem: T, count: Int = 1): Unit =
    buckets(elem).foreach { case (j, k) => counts(j)(k) += count }

  private def buckets(elem: T): Seq[(Int, Int)] = {
    val hash1 = elem.##
    val hash2 = math.abs(MurmurHash3.mixLast(0, hash1))

    (0 until d).map { i =>
      val h = hash1 + i * hash2
      val nextHash = if (h < 0) ~h else h
      (i, nextHash % w)
    }
  }
}
