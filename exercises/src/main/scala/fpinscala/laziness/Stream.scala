package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = {
    def take(n:Int,ns:Stream[A],s:Stream[A]):Stream[A]= s match {
      case Empty => ns
      case Cons(h,t) if(n>=1) => take(n-1,Cons(h,()=>ns),t.apply())
      case _ if(n<1) => ns
    }

    take(n,Stream(),this)

  }

  def drop(n: Int): Stream[A] = {
    def drop(n:Int,s:Stream[A]):Stream[A]= s match {
      case Empty => s
      case Cons(h,t) if(n>=1) => drop(n-1,t.apply())
      case _ if(n<1) => s
    }

    drop(n,this)

  }

  def takeWhile(p: A => Boolean): Stream[A] = foldRight(Stream[A]())((a,b)=>if(p(a)) cons(a,b) else empty)

  def forAll(p: A => Boolean): Boolean =this match {
    case Empty=>true
    case Cons(h,t) if(!p(h.apply())) => false
    case Cons(h,t) => t.apply().forAll(p)
  }

  /*{
    def forAll(p: A => Boolean,s:Stream[A]):Boolean=s match {
      case Empty=>true
      case Cons(h,t) if(!p(h.apply())) => false
      case Cons(h,t) => forAll(p,t.apply())
    }
    forAll(p,this)
  }*/

  def headOption: Option[A] = sys.error("todo")

  def toList: List[A] ={

    def toList(l:List[A],s:Stream[A]):List[A]= s match {
      case Empty=>l
      case Cons(h,t)=> toList( l ++ List(h.apply()),t.apply())
    }

    toList(List(),this)
  }

  def map[B](f:A => B):Stream[B]=foldRight(Stream[B]())((a,b)=>cons(f(a),b))

  def filter(p:A => Boolean):Stream[A]=foldRight(Stream[A]())((a,b) => if(!p(a)) cons(a,b) else b)

  def append[B>:A](s: => Stream[B]):Stream[B]=foldRight(s)((a,b)=>cons(a,b))

  def flatMap[B>:A](f:A => Stream[B]):Stream[B]=foldRight(Stream[B]())((a,b)=>f(a).append(b))


  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def   empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = unfold(1)((x:Int)=>Some((1,1)))
  def from(n: Int): Stream[Int] = unfold(n:Int)((x:Int)=>Some((x+1,x+1)))

  val fibs:Stream[Int]=unfold((0,1))((x:(Int,Int))=>Some(x._1,(x._2,x._1+x._2)))

  private def fibs(a:Int,b:Int):Stream[Int]=cons(a+b,fibs(b,a+b))

  def constant(n:Int):Stream[Int]=unfold(n:Int)((x:Int)=>Some((x,x)))


  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case Some(a)=>cons(a._1,unfold(a._2)(f))
      case None => empty
    }


}