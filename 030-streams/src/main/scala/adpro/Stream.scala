// Advanced Programming
// Andrzej Wąsowski, IT University of Copenhagen
//
// meant to be compiled, for example: fsc Stream.scala


//In pattern matching check the most likely case first to improving performance


package adpro

sealed trait Stream[+A] {
  import Stream._

  def headOption () :Option[A] = this match {
      case Cons(h,t) => Some(h())
      case Empty => None
    }
    
  def tail :Stream[A] = this match {
      case Cons(h,t) => t()
      case Empty => Empty
  }

  def foldRight[B] (z : =>B) (f :(A, =>B) => B) :B = this match {
      case Cons (h,t) => f (h(), t().foldRight (z) (f))
      case Empty => z
      // Note 1. f can return without forcing the tail
      // Note 2. this is not tail recursive (stack-safe) It uses a lot of stack
      // if f requires to go deeply into the stream. So folds sometimes may be
      // less useful than in the strict case
    }

  // Note 1. eager; cannot be used to work with infinite streams. So foldRight
  // is more useful with streams (somewhat opposite to strict lists)
  def foldLeft[B] (z : =>B) (f :(A, =>B) =>B) :B = this match {
      case Cons (h,t) => t().foldLeft (f (h(),z)) (f)
      case Empty => z
      // Note 2. even if f does not force z, foldLeft will continue to recurse
    }

  def exists (p : A => Boolean) :Boolean = this match {
      case Cons (h,t) => p(h()) || t().exists (p)
      case Empty => false
      // Note 1. lazy; tail is never forced if satisfying element found this is
      // because || is non-strict
      // Note 2. this is also tail recursive (because of the special semantics
      // of ||)
    }

  import scala.annotation._
  //Exercise 2
  //Question// Is there a way to make it tail recursive?
  /*
  The above solution will stack overflow for large streams, since it's
  not tail-recursive. Here is a tail-recursive implementation. At each
  step we cons onto the front of the `acc` list, which will result in the
  reverse of the stream. Then at the end we reverse the result to get the
  correct order again.
  */
  // def toList: List[A] = {
  //   @annotation.tailrec
  //   def go(s: Stream[A], acc: List[A]): List[A] = s match {
  //     case Cons(h,t) => go(t(), h() :: acc)
  //     case _ => acc
  //   }
  //   go(this, List()).reverse
  // }

  /*
  In order to avoid the `reverse` at the end, we could write it using a
  mutable list buffer and an explicit loop instead. Note that the mutable
  list buffer never escapes our `toList` method, so this function is
  still _pure_.
  */
  // def toListFast: List[A] = {
  //   val buf = new collection.mutable.ListBuffer[A]
  //   @annotation.tailrec
  //   def go(s: Stream[A]): List[A] = s match {
  //     case Cons(h,t) =>
  //       buf += h()
  //       go(t())
  //     case _ => buf.toList
  //   }
  //   go(this)
  // }
  //@tailrec

  def toList: List[A] = this match{
    case Cons(h,t) => h()::t().toList
    case _ => List()  //Nil
  }

  //def toList: List[A] = foldRight(List.empty[A])((a, b) => a::b)

  //Exercise 3
  // Note: Whenever you use if statement you must take care of else (i.e what if your if statment was not true)
  def take(n: Int): Stream[A] = this match{
    case Cons(h,t) => if (n > 0) cons(h(), t().take(n-1)) else Empty
    case Empty => Empty
  }

  def drop(n: Int): Stream[A] = this match{
     case Empty => Empty
     case Cons(h, t) if (n>0) =>  t().drop(n-1)
     case _ => this
   }
//  this match{  Wrong
//    case Cons(h,t) => if (n>1) t().drop(n-1) else t() // or instead of t(), this
//    case Empty => Empty
//  }
  //Exercise 4
  def takeWhile(p: A => Boolean): Stream[A] = 
    this match{
//    case Cons(h,t) => if (p(h())) cons(h(), t().takeWhile(p)) else Empty
      case Cons(h,t) if (p(h())) => cons(h(), t().takeWhile(p))
      case _ => Empty

      //      case Empty => Empty
  }


  //Exercise 5
  //Question I was testing if it is tail recursive but compiler complains it id nether
  //final nor private. when i made it final it was fine at compile time
  
  
  //but when i ran tests it fail in loop and my cpu usage was 100%?  
  //1 is the first natural number and there is no last natural number.
  //This fact is expressed by saying that there are infinitely many natural numbers.
  //Therefore it is not stack safe.
  // The < 0 case is fine since it return false at the begining and wont continue
  //@tailrec
  /*final*/ def forAll(p: A => Boolean): Boolean = 
  // this match{
  //   case Empty => true
  //   case Cons(h,t) => if(p(h())) t().forAll(p) else false
  // }

  foldRight(true)((a,b) => p(a) && b)
  //foldRight(true)(p(_) && _)



  //Exercise 6
  //Question// What is diffrence between Empty and empty?! Nothing.


  //Question How should I know that here it is critial to write type parameter Stream[A]?
  //If I do not meantion it, compiler will complain:
  //sbt:030-streams> compile
// [info] Compiling 1 Scala source to /media/neutron/D/3Semester/AP/Repository/Todo/030-streams/target/scala-2.12/classes ...
// [error] /media/neutron/D/3Semester/AP/Repository/Todo/030-streams/src/main/scala/adpro/Stream.scala:111:49: type mismatch;
// [error]  found   : adpro.Stream[A]
// [error]  required: adpro.Empty.type
// [error]   this.foldRight(Empty)((a, b) => if (p(a)) cons(a, b) else empty)
// [error]                                                 ^
// [error] /media/neutron/D/3Semester/AP/Repository/Todo/030-streams/src/main/scala/adpro/Stream.scala:111:61: polymorphic expression cannot be instantiated to expected type;
// [error]  found   : [A]adpro.Stream[A]
// [error]  required: adpro.Empty.type
// [error]   this.foldRight(Empty)((a, b) => if (p(a)) cons(a, b) else empty)
// [error]                                                             ^
// [error] two errors found
// Any tips?!

// If I write [Stream[B]]:
// not found: type B
// [error]   this.foldRight[Stream[B]](Empty)((a, b) => if (p(a)) cons(a, b) else empty)
// [error]    

// if i write empty[A] in initial value position it works fine


  def takeWhile2(p: A => Boolean): Stream[A] = 
  this.foldRight(empty[A])((a, rest) => if (p(a)) cons(a, rest) else empty)
  //recall foldRight
  //def foldRight[B] (z : =>B) (f :(A, =>B) => B) :B = this match {
      //case Empty => z
      //case Cons (h,t) => f (h(), t().foldRight (z) (f))

  //Exercise 7
  def headOption2 () :Option[A] =
  this.foldRight[Option[A]](None) ((a,b) => Some(a))

  //Exercise 8 The types of these functions are omitted as they are a part of the exercises
  def map[B](f: A => B) : Stream[B] = 
  foldRight[Stream[B]](empty)((a,b) => cons(f(a), b))

  //this match{
  //   case Empty => Empty
  //   case Cons(h, t) => cons(f(h()), t().map(f))
  // }

  def filter[B](p: A => Boolean) : Stream[A] = 
  foldRight[Stream[A]](empty)((a,b) => if (p(a)) cons(a, b) else b    )

  // Question// what is the type of t? Stream[B]
  def append[B >: A](that: => Stream[B]): Stream[B] =
    foldRight[Stream[B]](that: Stream[B])((h: A, t) => cons(h, t))


  def flatMap[B](f: A => Stream[B]) : Stream[B] = 
      foldRight(empty[B])((h, t) => f(h) append t )

    //foldRight[Stream[B]](empty)((a,b) => f(a) append t)


  //Exercise 09
  //Put your answer here:

//Because Stream is lazy so it will be terminated and returns current element once the p returns true.
// i.e., it won't check if p holds for all elements of a Stream immediately.It will check step by step, 
//meaning it checks if p  return true for the first element then terminates and return the value of the first element,
// otherwise check the second element and so on.
//On the other hand, it will first check whether p holds for all the elements of a list and then collects all the elements 
//that satisfy the prediction function p. In the end, it returns the headOption of the collected elements.  

  //Exercise 10
  //Put your answer here:
  // def fibs: Stream[Int] =
  // naturals.foldRight[Stream[Int]](empty)(  (element: Int ,accumulator) => cons(
  //   accumulator.headOption().getOrElse(1) + accumulator.tail.headOption().getOrElse(0), 
  //   accumulator  )
  // def fibs: Stream[Int] =
  //    {
  //     def go[n](current: Int, next: Int): Stream[Int] =  {
  //       //if (current <= 0)   cons(0, empty )
  //        //else if (current == 1)    
  //        cons(current, go(current = current + next, next = current))
  //        //else {
  //          //println(current)
  //         // go(n - 1, current = current + next, next = current)
  //       //}
  //       }
  //       go(current = 0, next = 1)
  //    }
  

  //Exercise 11
  //unfold is just a generator of Streams based on state
  //Now we're going to write a more general stream-building function: unfold which takes an initial state,
  //and a function for building both the next state and the next value in the stream to be generated:

  /*
  Exercise 11. Write a more general stream-building function called unfold . It takes an initial state,
  and a function for producing both the next state and the next value in the generated stream.
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A]
  If you solve it without using pattern matching, then you obtain a particularly concise solution, that
  combines aspects of this and last week’s material.
  Test this function by unfolding the stream of natural numbers and checking whether its finite prefix
  is equal to the corresponding prefix of naturals .
  */

   def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
   //    f(z) match {
   //    case Some((a, s)) => cons(a, unfold(s)(f))
   //    case None => empty
   //  }
   ///The below two implementations use `fold` and `map` functions in the Option class to implement unfold,
   ///thereby doing away with (get rid of something or stop using it) the need to manually pattern match as in the above solution.
   //    f(z).map { case (a, b) => cons(a, unfold(b)(f)) }.getOrElse[Stream[A]](Stream.empty)
   //    f(z).map((p: (A,S)) => cons(p._1,unfold(p._2)(f))).getOrElse(empty[A])
     f(z).fold (empty[A]) ((p: (A,S)) => cons(p._1,unfold(p._2)(f)))

//   def unfold[A, B](a: A)(f: A => Option[(A, B)]): Stream[B] =
//   f(a).map { case (a, b) => b #:: unfold(a)(f) }.getOrElse(Stream.empty)

//   def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
//     case Some((head, state)) => Stream.cons(head, unfold(state)(f))
//     case None => Stream.empty[A]
//   }

  //Exercise 12
  def fib2 : Stream[Int] = unfold((0,1)) {   case (a,s) => Some( (a, (s, a+s)   )   )}

//   def fib2  = unfold((0, 1))(pair => {
//     val (a, b) = pair
//     Some(a, (b, a+b))
// })

  // def fib2: Stream[Int] = unfold((0, 1)) {
  //   case (i, j) => Some( (i, (j, i + j)) )
  // }

  //def from2 : Stream[Int]= unfold((0,1)) {  case (a, s) =>  Some( (a, (s, a+1 )))   }

  //Exercise 13

  // def map[B](f: A => B) : Stream[B] = 
  // foldRight[Stream[B]](empty)((a,b) => cons(f(a), b))

  def map2[B](f: A => B): Stream[B] = unfold (this) (s => s.headOption.map (a => (f(a),s.tail)))
//   unfold(this) {
//     case Cons(h, t) => Some((f(h()), t()))
//     case _ => None
//       //case Cons(h, t) => cons(f(h()), t().map2(f))
// }

// val h = s.headOption
//     h match {
//        Some(c) => Some(c, s.tail) //We luckily have a way to get the tail out of a Stream :)
//        None _  => None
//     }



  // def take(n: Int): Stream[A] = this match{
  //   case Empty => Empty
  //   case Cons(h,t) => if (n > 0) cons(h(), t().take(n-1)) else Empty
  // }
  def take2 (n: Int): Stream[A] =
  unfold((this, n)) {
    case (Cons(h, t), 1) => Some((h(), (empty, 0)))
    case (Cons(h, t), i) if i > 1 => Some((h(), (t(), i - 1)))
    case _ => None
  }


  def takeWhile3 (p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }

  def zipWith2[B, C] (f: (A, B) => C) (s2: Stream[B]): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    } 

}


case object Empty extends Stream[Nothing]
case class Cons[+A](h: ()=>A, t: ()=>Stream[A]) extends Stream[A]

object Stream {

  def empty[A]: Stream[A] = Empty

  def cons[A] (hd: => A, tl: => Stream[A]) :Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  //If you want to have toList on Stream i.e if you what to access toList with Stream.toList
  //you should implement it in the companion object (here).
  def toList111[A](s: Stream[A]): List[A] = s match{
    case Cons(h,t) => h():: toList111(t())
    case _ => List()  //Nil

  }

  def apply[A] (as: A*) :Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
    // Note 1: ":_*" tells Scala to treat a list as multiple params
    // Note 2: pattern matching with :: does not seem to work with Seq, so we
    //         use a generic function API of Seq


  //Exercise 1
  def from(n:Int):Stream[Int]=cons(n,from(n+1))

  def to(n:Int):Stream[Int]= cons(n,from(n-1))

  val naturals: Stream[Int] = from(0)



  def fibs: Stream[Int] = {
      def go(current: Int, next: Int): Stream[Int] =  {
         cons(current, go(current = current + next, next = current))
        }
        go(current = 0, next = 1)
      }
      

     //val fibs(): Stream[Int] = {
  //def start(prev : Int , next : Int): Stream[Int]={
  //cons(prev,start(next,next+prev))}
  //start(0,1)

  // 

  // def fib (n: Int) : Int ={
  //   @annotation.tailrec
  //    def go(n: Int, current: Int, next: Int): Int =  {
  //   if (n <= 0)   { 0 }
  //    else if (n == 1)    {current}
  //    else {
  //      //println(current)
  //      go(n - 1, current = current + next, next = current)
  //   }
  //   }
  //   go(n, current = 0, next = 1)
  // }


  // def fib2 : Stream[Int] = 
  // unfold((0,1)) {   case (a,s) => Some( (a, (s, a+s)   )   )}

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
  //    f(z) match {
//    case Some((a, s)) => cons(a, unfold(s)(f))
//    case None => empty
//  }

  /*
  The below two implementations use `fold` and `map` functions in the Option class to implement unfold,
  thereby doing away with (get rid of something or stop using it) the need to manually pattern match as in the above solution.
   */
//    f(z).map { case (a, b) => cons(a, unfold(b)(f)) }.getOrElse[Stream[A]](Stream.empty)
//    f(z).map((p: (A,S)) => cons(p._1,unfold(p._2)(f))).getOrElse(empty[A])
    f(z).fold (empty[A]) ((p: (A,S)) => cons(p._1,unfold(p._2)(f)))

  def fib2: Stream[Int] = Stream.unfold((0, 1)) {
    case (i, j) => Some( (i, (j, i + j)) )
  }

  def from2 (n: Int): Stream[Int]= unfold((n)) {  a =>  Some(a, a+1)   }

}

