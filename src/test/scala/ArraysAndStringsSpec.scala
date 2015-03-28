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

  "isAnagram" should {
    "check if two strings are anagrams" in {
      isAnagram("anagram", "magarna") must_== true
      isAnagram("Henrik Schyffert", "Fin tysk herrchef") must_== true
      isAnagram("anagramtjänst", "jama ansträngt") must_== true
    }
  }

  "rotate image" should {
    "rotate an image" in {

      val pixels = Array(
        Array(0, 1),
        Array(2, 3)
      )

      val rotatedPixels = Array(
        Array(2, 0),
        Array(3, 1)
      )

      val img = new Image(pixels)
      val rotated = new Image(rotatedPixels)
      rotate(img).pixels must_== rotated.pixels
    }
  }
}
