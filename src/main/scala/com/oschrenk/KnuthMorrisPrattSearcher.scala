package com.oschrenk

import scala.annotation.tailrec

object KnuthMorrisPrattSearcher {
  val NotFound = -1
}

class KnuthMorrisPrattSearcher {

  import KnuthMorrisPrattSearcher._

  def position(haystack: String, needle: String): Int = {
    if (needle == null || needle.isEmpty) {
      return NotFound
    }

    val t = table(needle)

    def f(m: Int, i: Int): Int = {
      if (m + i < haystack.length) {
        if (needle(i) == haystack(m + i)) {
          if (i == needle.length - 1) {
            m
          } else {
            f(m, i + 1)
          }
        } else {
          if (t(i) > -1) {
            f(m + i - t(i), t(i))
          } else {
            f(m + 1, 0)
          }
        }
      } else {
        -1
      }
    }

    f(0, 0)
  }

  def table(needle: String): Seq[Int] = {
    val t = Array(-1, 0) ++ Array.fill(needle.length - 2)(0)

    @tailrec
    def f(pos: Int, cnd: Int): Array[Int] = {
      if (pos < needle.length) {
        if (needle(pos - 1) == needle(cnd)) {
          t(pos) = cnd + 1
          f(pos + 1, cnd + 1)
        } else if (cnd > 0) {
          f(pos, t(cnd))
        } else {
          t(pos) = 0
          f(pos + 1, cnd)
        }
      } else {
        t
      }
    }

    f(2, 0)
  }
}
