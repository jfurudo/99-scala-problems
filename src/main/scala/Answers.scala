package ans

object Answers {

  def printAnswer[T, U](lst: List[T], result: U): Unit = {
    println("Input: " + lst.toString)
    println("Output: " + result)
  }

  def foldLeft[T, U](xs: List[T])(z: U)(f: (U, T) => U): U=
    xs match {
      case Nil => z
      case h :: tail => foldLeft(tail)(f(z, h))(f)
    }

  def takeWhile[T](lst: List[T])(f: T => Boolean): List[T] = 
    foldLeft(lst)((Nil: List[T], true))((x, y) =>
      if (f(y) && x._2) (y :: x._1, true) else (x._1, false))._1.reverse

  // P01
  def last[T](lst: List[T]): T =
    if (lst.tail == Nil) lst.head else last(lst.tail)

  // P02
  def penultimate[T](lst: List[T]): T =
    lst match {
      case a :: b :: Nil => a
      case _ => penultimate(lst.tail)
    }

  // P03
  def nth[T](n: Int, lst: List[T]): T =
    if (n == 1) lst.head else nth(n - 1, lst.tail)

  // P04
  def mylength[T](lst: List[T]): Int =
    if (lst == Nil) 0 else 1 + mylength(lst.tail)

  // p05
  def reverse[T](lst: List[T]): List[T] =
    if (lst == Nil) Nil: List[T] else reverse(lst.tail) ++ List[T](lst.head)

  // p06
  def isPalindrome[T](lst: List[T]): Boolean =
    if (lst.isEmpty || lst.length == 1) true
    else if  (lst.head == lst.last) isPalindrome(lst.tail.reverse.tail)
    else false

  // P07
  def flatten[T](lst: List[T]): List[Any] = {
    lst match {
      case Nil => Nil
      case (nsLst: List[Any]) :: tail => flatten(nsLst) ++ flatten(tail)
      case h :: tail => h :: flatten(tail)
    }
  }

  // p08
  def compress[T](lst: List[T]): List[T] = {
    lst match {
      case Nil => Nil: List[T]
      case a :: b :: tail if a == b => compress(a :: tail)
      case a :: tail => a :: compress(tail)
    }
  }

  // P09 (**) Pack consecutive duplicates of list elements into sublists.
  //   If a list contains repeated elements they should be placed in separate sublists.
   
  //   Example:
   
  //   scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  //   res0: List[List[Symbol]] =
  // List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))

  // わからん
  def pack[T](lst: List[T]): List[List[Any]] = ???
}
