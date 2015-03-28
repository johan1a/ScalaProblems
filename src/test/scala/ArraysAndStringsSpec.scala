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

  "reverseCString" should{
    "reverse strings that are terminated with the null character (endl)" in {
      reverseCString("abcd\n".toCharArray) must_== "dcba\n".toCharArray
    }
  }
}
