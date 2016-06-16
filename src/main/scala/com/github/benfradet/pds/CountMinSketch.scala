package com.github.benfradet.pds

import scala.math._
import scala.util.hashing.MurmurHash3

class CountMinSketch[T](w: Int, d: Int) {
  def this(eps: Double, delta: Double) =
    this(ceil(2 / eps).toInt, ceil(-log(1 - delta) / log(2)).toInt)

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
