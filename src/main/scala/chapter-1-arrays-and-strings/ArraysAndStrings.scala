
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
    for ((char, i) <- sortedStr.zipWithIndex drop (1)) {
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
    str.takeWhile((c: Char) => c != NullChar).reverse.toArray :+ NullChar
  }

}