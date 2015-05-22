import org.scalatest._
import ans.Answers._

class AnswersSpec extends FlatSpec with Matchers {
  val nlist = List(1, 1, 2, 3, 5, 8)
  val palidromeList = List(1, 2, 3, 4, 3, 2, 1)
  val nestedList = List(List(1, 1), 2, List(3, List(5, 8)))
  val symbolList = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  

  "foldLeft" should "my foldLeft function" in {
    foldLeft(nlist)(0)((x, y) => x + y) should be (20)
    foldLeft(nlist)(Nil: List[Int])((x, y) => y :: x) should be (nlist.reverse)
  }

  "takeWhile" should "my takeWhile function" in {
    takeWhile(nlist)(_ < 3) should be (nlist.takeWhile(_ < 3))
  }

  "p1" should "last" in {
    last(nlist) should be (8)
  }

  "p2" should "penultimate" in {
    penultimate(nlist) should be (5)
  }

  "p3" should "nth" in {
    nth(3, nlist) should be (2)
  }

  "p4" should "length" in {
    mylength(nlist) should be (6)
  }

  "p5" should "reverse" in {
    reverse(nlist) should be (nlist.reverse)
  }

  "p6" should "isPalidrome" in {
    isPalindrome(nlist) should be (false)
    isPalindrome(palidromeList) should be (true)
  }

  "p7" should "flatten" in {
    flatten(nestedList) should be (List(1, 1, 2, 3, 5, 8))
  }

  "p8" should "compose" in {
    compress(symbolList) should be (List('a, 'b, 'c, 'a, 'd, 'e))
  }

  "p9" should "pack" in {
//     pack(symbolList) should be (List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
  }
}
