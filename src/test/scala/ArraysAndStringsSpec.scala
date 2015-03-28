import ArraysAndStrings._

import org.specs2.mutable.Specification
/**
 * Created by johan on 2015-03-28.
 */
class ArraysAndStringsSpec extends Specification { override def is = s2"""

  The allUniqueChars function should
    return true for strings with distinct characters  $e1
    return false for strings with repeated characters $e2
                                                      """
  def e1 = allUniqueChars("abcdef") must_== true
  def e2 = allUniqueChars("abcdefa") must_== false

}
