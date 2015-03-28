
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
}