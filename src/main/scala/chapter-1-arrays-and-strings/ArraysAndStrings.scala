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


}