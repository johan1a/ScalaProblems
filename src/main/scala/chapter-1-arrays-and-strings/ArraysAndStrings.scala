import scala.language.postfixOps

object ArraysAndStrings {

  /*
   * 1.1 Implement an algorithm to determine
   * if a string has all unique characters.
   * What if you can not use additional data structures?
   */
  def allUniqueChars(str: String): Boolean = {
    str.distinct equals str
  }

  def allUniqueChars2(str: String): Boolean = {
    val sortedStr = str.sorted
    var prev: Char = sortedStr(0)
    var result = true
    for ((char, i) <- sortedStr.zipWithIndex drop 1) {
      if (char == prev) {
        result = false
      }
      prev = char
    }
    result
  }

  /*
  * 1.2 Write code to reverse a C-Style String. (C-String means that “abcd” is represented as
  * five characters, including the null character.)
  */

  val NullChar: Char = '\n'

  def reverseCString(str: Seq[Char]): Array[Char] = {
    ((str takeWhile ((c: Char) => c != NullChar) reverse) toArray) :+ NullChar
  }

  /*
  * 1.3 Design an algorithm and write code to remove the duplicate characters in a string
  * without using any additional buffer. NOTE: One or two additional variables are fine.
  * An extra copy of the array is not.
  * FOLLOW UP
  * Write the test cases for this method.
  */
  def removeDuplicates(str: String): String = {
    str.distinct
  }

  import scala.util.control.Breaks._

  def removeDuplicates2(str: String): String = {
    var mutableStr = str
    var i: Int = 0
    while (i < mutableStr.length) {
      val firstOccurrence: Char = str(i)
      breakable {
        for ((char, itr) <- mutableStr.zipWithIndex drop i + 1) {
          if (char == firstOccurrence) {
            mutableStr = mutableStr.take(itr) + mutableStr.drop(itr + 1)
          }
        }
      }
      i += 1
    }
    mutableStr
  }

  /*
  * 1.4 Write a method to decide if two strings are anagrams or not.
  */
  def isAnagram(str1: String, str2: String): Boolean = {
    val a = prepForAnagramCheck(str1)
    val b = prepForAnagramCheck(str2)
    prepForAnagramCheck(str1) == prepForAnagramCheck(str2)
  }

  def prepForAnagramCheck(str: String): String = {
    str.toLowerCase.filter((c: Char) => !c.isWhitespace).sorted
  }

  /*
  * 1.5 Write a method to replace all spaces in a string with ‘%20’
   */
  def replaceWhiteSpace(str: String): String = {
    str.replace("\n", "%20")
  }

  /*
  * 1.6 Given an image represented by an NxN matrix, where each pixel in the image is 4
  * bytes, write a method to rotate the image by 90 degrees. Can you do this in place?
   */

  type Pixel = Int

  class Image(var pixels: Array[Array[Pixel]]) {

    def this(n: Int) {
      this(Array.ofDim[Pixel](n, n))
    }
  }

  def rotate(img: Image): Image = {
    val n = img.pixels.length
    val rotated = new Image(n)
    for (j <- 0 until n) {
      for (i <- 0 until n) {
        rotated.pixels(j)(i) = img.pixels(n - 1 - i)(j)
      }
    }
    rotated
  }


}