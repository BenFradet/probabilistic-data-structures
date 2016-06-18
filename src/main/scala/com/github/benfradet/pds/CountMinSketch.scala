package com.github.benfradet.pds

import scala.math._
import scala.util.hashing.MurmurHash3

/**
 * From: https://en.wikipedia.org/wiki/Count%E2%80%93min_sketch
 *
 * CountMinSketch counts the frequency of events in a stream of data.
 *
 * It uses hash functions to map events to frequencies, using only sub-linear space, at the cost
 * of overcounting some events due to collisions.
 *
 * The goal is to consume a stream of events, one at a time, and count the frequency of the
 * different types of events in the stream.
 * At any time, the sketch can be queried for frequency of a particular event type ''i'', and
 * will return an estimate of this frequency that is within a certain distance of the true
 * frequency, with a certain probability.
 *
 * The underlying data structure is a two-dimensional array of ''w'' columns and ''d'' rows. The
 * parameters ''w'' and ''d'' are fixed when the sketch is created, and determine the time and
 * space needs and the probability of error when the sketch is queried. Associated with each of
 * the ''d'' rows is a separate hash function. The hash functions must be pairwise independent.
 * The parameters ''w'' and ''d'' can be chosen by setting ''w = ceil(e / eps)'' and
 * ''d = ceil(1 / delta)'', where the error in answering a query is within a factor of ''eps''
 * with probability ''delta''.
 *
 * When a new event of type ''i'' arrives we update as follows: for each of row ''j'' of the
 * table, apply the corresponding hash function to obtain a column index ''k = h_j(i)''. Then
 * increment the value in row ''j'', column ''k'' by one.
 *
 * The point query asks for the count of event type ''i''. The estimated count is given by the
 * least value in the table for i, namely ''â_i = min_j count[j, h_j(i)]'' where ''count'' is
 * the table. This estimate has the guarantee that ''â_i <= a_i + eps sum_j^n(a_j)'' with
 * probability ''1 - delta'' where ''a_i'' is the true frequency with which ''i'' occurred in the
 * stream.
 *
 * This implementation has been by inspired Algebird's:
 * https://github.com/twitter/algebird/blob/develop/algebird-core/src/main/scala/com/twitter/algebird/CountMinSketch.scala
 *
 * Reference paper: http://dimacs.rutgers.edu/~graham/pubs/papers/cmencyc.pdf
 */
class CountMinSketch[T](w: Int, d: Int) {
  def this(eps: Double, delta: Double) =
    this(ceil(exp(1) / eps).toInt, ceil(-log(1 - delta) / log(2)).toInt)

  private val counts = Array.ofDim[Long](d, w)

  private def buckets(elem: T): Seq[(Int, Int)] = {
    val hash1 = elem.##
    val hash2 = math.abs(MurmurHash3.mixLast(0, hash1))

    (0 until d).map { i =>
      val h = hash1 + i * hash2
      val nextHash = if (h < 0) ~h else h
      (i, nextHash % w)
    }
  }

  def apply(elem: T): Long = getCount(elem)

  def getCount(elem: T): Long =
    buckets(elem)
      .map { case (j, k) => counts(j)(k) }
      .min

  def add(elem: T, count: Int = 1): Unit =
    buckets(elem).foreach { case (j, k) => counts(j)(k) += count }
}
