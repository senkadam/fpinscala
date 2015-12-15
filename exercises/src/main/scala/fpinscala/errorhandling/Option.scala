package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter
import java.util.regex._

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(x)=> Some(f(x))
    case None => None
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(x)=> f(x)
    case None => None
  }


  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case Some(a)=> this
    case None=>ob
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if (f(a)) => this
    case Some(a) => None
    case None => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a,b) match {
    case (_,None)=>None
      case(None,_)=>None
      case(Some(x),Some(y))=> Some(f(x,y))
  }


  def sequence[A](a: List[Option[A]]): Option[List[A]] = {

    def makeList(a: List[Option[A]]): List[A]= a match {
      case Some(b) :: Nil => List(b)
      case Some(b) :: tail => b :: makeList(tail)
      case Nil => List()
      case None :: tail => throw new Exception("fail!")
      case List(None) => throw new Exception("fail!")

    }
    try {Some(makeList(a))} catch { case e:Exception=>None}

  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    def makeList(a: List[A]): List[B] ={
      def error(x:Option[B])=x match {
        case None =>  throw new Exception("fail!")
        case Some(x) => x
      }

      a match {

        case b :: Nil => List(error(f(b)))
        case b :: tail =>error(f(b)) :: makeList(tail)
        case Nil => List()
        case None :: tail => throw new Exception("fail!")
        case List(None) => throw new Exception("fail!")

      }
    }

    try {
      Some(makeList(a))
    } catch {
      case e: Exception => None
    }
  }


  def variance(xs:Seq[Double]):Option[Double]={
    val m= mean(xs) match {
      case Some(a)=> a
      case None=> return None
    }
    xs.map((a:Double) =>math.pow(a-m,2))
    return mean(xs.map((a:Double) =>math.pow(a-m,2)))
  }

  def bothMatch(a:String,b:String,s:String):Option[Boolean]={
    map2(mkMatcher(a),mkMatcher(b))((x:String=>Boolean,y:String=>Boolean)=>(x(s) && y(s)))
  }

  def mkMatcher(pat: String): Option[String => Boolean] =
   pattern(pat) map ((p) => (s:String)=>p.matcher(s).matches)

  def pattern(s:String): Option[Pattern] = try {
    Some(Pattern.compile(s))
  }catch{
    case e: PatternSyntaxException => None
  }
}