package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def appendFold[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1,a2)((a,b)=>Cons(a,b))

  def appendLists[A](l: List[List[A]]): List[A] = {
    val r: List[A]=Nil
    foldRight(l,r)((a,b)=>List.append(a,b))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldLeft(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_ , x) => x
  }

  def setHead[A](l: List[A], h: A): List[A] = Cons(h,tail(l));

  def drop[A](l: List[A], n: Int): List[A] = n match {
    case _ if (n<1) => l
    case _ => drop(List.tail(l),n-1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x,y) if(f(x))=> dropWhile(y,f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = {
    def appendUntilOne(l1:List[A],l2:List[A]) :List[A]= l2 match {
      case Nil => Nil
      case Cons(x,Nil)=>l1
      case Cons(x,y)=>appendUntilOne(List.append(l1,List(x)),List.tail(l2))
    }

    appendUntilOne(Nil,l)
  }

  def length[A](l: List[A]): Int = foldLeft(l,0)((x,y)=>(x + 1))

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x,Nil)=> f(z,x)
    case Cons(x,y) => f(foldLeft(y,z)(f),x)
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = l match  {
    case Nil => Nil
    case Cons(a,Nil)=>Cons(f(a),Nil)
    case Cons(a,b) => Cons(f(a),List.map(b)(f))
  }

  def reverse[A](l: List[A]) = {
    val r:List[A]=Nil
    foldLeft(l,r)((a,b)=>List.append(a,List(b)))
  }

  def filter[A](l: List[A])(f : A => Boolean):List[A] = l match {
    case Nil => Nil
    case Cons(a,b) if (f(a))=> Cons(a,List.filter(b)(f))
    case Cons(a,b) => List.filter(b)(f)
  }

  def filterFM[A](l: List[A])(f : A => Boolean):List[A] = flatMap(l)((a)=>if(f(a)) List(a) else Nil)

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match  {
    case Nil => Nil
    case Cons(a,b) => List.append(f(a),(List.flatMap(b)(f)))
  }

  def addLists[A](l1:List[A],l2:List[A])(f: (A,A)=>A):List[A]=(l1,l2) match {
    case (Nil,Nil) => Nil
    case(Cons(a,b),Nil) => Cons(a,addLists(b,Nil)(f))
    case(Nil,Cons(a,b)) => Cons(a,addLists(b,Nil)(f))
    case(Cons(a,b),Cons(c,d)) => Cons(f(a,c),addLists(b,d)(f))
  }

  def hasSubsequence[A](l:List[A],s:List[A]):Boolean = (l,s) match {
    case (Nil,Nil) => false
    case (_,Nil) => false
    case (Nil,_) =>false
    case (Cons(a,b),(Cons(c,Nil))) if(a==c) => true
    case(Cons(a,b),(Cons(c,d))) if(a==c) => hasSubsequence(b,d) || hasSubsequence(b,s)
    case(Cons(a,b),(Cons(c,d))) => hasSubsequence(b,s)
  }
}
