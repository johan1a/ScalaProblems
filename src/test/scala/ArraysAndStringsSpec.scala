import ArraysAndStrings._

import org.specs2.mutable.Specification

/**
 * Created by johan on 2015-03-28.
 */
class ArraysAndStringsSpec extends Specification {

  "allUniqueChars" should {
    "return true for strings with distinct characters" in {
      allUniqueChars("abcdef") must_== true
      allUniqueChars("abcdefa") must_== false
    }
  }

  "reverseCString" should {
    "reverse strings that are terminated with the null character (endl)" in {
      reverseCString("abcd\n".toCharArray) must_== "dcba\n".toCharArray
    }
  }

  "removeDuplicates" should {
    "remove duplicate characters in a string" in {
      removeDuplicates("abcda") must_== "abcd"
      removeDuplicates("abcd") must_== "abcd"
      removeDuplicates("") must_== ""
      removeDuplicates("aa") must_== "a"
      removeDuplicates("baa") must_== "ba"
      removeDuplicates("abba") must_== "ab"
      removeDuplicates("aab") must_== "ab"
    }
  }
  "removeDuplicates2" should {
    "remove duplicate characters in a string" in {
      removeDuplicates2("abcda") must_== "abcd"
      removeDuplicates2("abcd") must_== "abcd"
      removeDuplicates2("") must_== ""
      removeDuplicates2("aa") must_== "a"
      removeDuplicates2("baa") must_== "ba"
      removeDuplicates2("abba") must_== "ab"
      removeDuplicates2("aab") must_== "ab"
    }

  }
}
