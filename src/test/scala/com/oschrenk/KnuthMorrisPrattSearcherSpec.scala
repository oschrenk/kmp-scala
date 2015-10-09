package com.oschrenk

import org.scalatest._

class KnuthMorrisPrattSearcherSpec extends FlatSpec with Matchers {

  private val subject = new KnuthMorrisPrattSearcher()

  private val haystack = "foobar"

  "KnuthMorrisPratt" should "return `NotFound` for `null` needle" in {
    val needle = null

    subject.position(haystack, needle) shouldBe KnuthMorrisPrattSearcher.NotFound
  }

  it should "return `NotFound` for empty needle" in {
    val needle = ""

    subject.position(haystack, needle) shouldBe KnuthMorrisPrattSearcher.NotFound
  }

  it should "return `NotFound` for needle not in haystack" in {
    val needle = "baz"

    subject.position(haystack, needle) shouldBe KnuthMorrisPrattSearcher.NotFound
  }

  it should "return `0` for needle at beginning of haystack" in {
    val needle = "foo"

    subject.position(haystack, needle) shouldBe 0
  }

  it should "return correct position of needle in haystack" in {
    val needle = "bar"

    subject.position(haystack, needle) shouldBe 3
  }
}
