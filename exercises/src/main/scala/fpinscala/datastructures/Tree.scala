package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t:Tree[A]):Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }

  def maximum(t:Tree[Int]):Int=t match {
    case Leaf(a) => a
    case Branch(l,r) => maximum(l) max maximum(r)
  }

  def depth(t:Tree[Int]):Int=t match {
    case Leaf(a) => 1
    case Branch(l,r) => 1 + (depth(l) max depth (r))
  }

  def map[A,B](t:Tree[A])(f: A => B):Tree[B]= t match {
    case Leaf(a) =>Leaf(f(a))
    case Branch(l,r)=> Branch(map(l)(f),map(r)(f))
  }

  def fold[A](t:Tree[A],fa:A=>A)(f: (A,A)=>A):A = t match {
    case Leaf(z) => fa.apply(z)
    case Branch(l,r) =>f(fold(l,fa)(f),fold(r,fa)(f))
  }

  def maximumFold(t:Tree[Int]):Int={

    fold(t,(a:Int)=>a)((a,b)=> a max b)
  }

  def depthFold(t:Tree[Int]):Int=fold(t,((a:Int)=>1))((a,b)=> (a max b) + 1)

  def sizeFold(t:Tree[Int]):Int =fold(t,((a:Int)=>1 ))((a,b)=> (a + b) + 1)



}