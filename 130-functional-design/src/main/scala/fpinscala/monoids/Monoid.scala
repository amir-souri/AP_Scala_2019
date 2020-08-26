// Advanced Programming 2015
// Andrzej Wasowski, IT University of Copenhagen
package fpinscala.monoids
import scala.language.higherKinds

trait Monoid[A] {

  def op(a1: A, a2: A): A
  def zero: A

}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  // I guess such objects could be useful if we combine them with implicits

  def listMonoid[A] = new Monoid[List[A]] {
    def op (a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  // Exercise 1

   val intAddition = new Monoid[Int] {
   def op(a:Int , b:Int): Int = a + b
   val zero = 0
  }

   val intMultiplication = new Monoid[Int] {
   def op(a:Int, b:Int): Int = a * b
  val zero = 1
   }

   val booleanOr = new Monoid[Boolean] {
   def op (a: Boolean, b: Boolean): Boolean = a || b
   val zero = false
  }

   val booleanAnd = new Monoid[Boolean] {
     def op (a: Boolean, b: Boolean): Boolean = a && b
     val zero = true
    }

  // Exercise 2

   def optionMonoid[A] = new Monoid[Option[A]] {
   def op(l: Option[A], r: Option[A]): Option[A] = l orElse r
     //if(l.isDefined) l else r //if(! l.isEmpty) l else r
   val zero = None
   }


  /*  a1.orElse(None) = a1  "right unit"
      and
      None.orElse(a2) = a2  "left unit" */

   /*Notice that we have a choice in how we implement `op`.
   We can compose the options in either order. Both of those implementations
   satisfy the monoid laws, but they are not equivalent.
   This is true in general--that is, every monoid has a _dual_ where the
   `op` combines things in the opposite order. Monoids like `booleanOr` and
   `intAddition` are equivalent to their duals because their `op` is commutative
   as well as associative.

   We can get the dual of any monoid just by flipping the `op`.*/

  def dual[A] (m :Monoid[A]) = new Monoid[A] {
    def op (a1: A, a2: A) = m.op(a2,a1)
    val zero = m.zero
  }

  //// Now we can have both monoids on hand:
  ///def firstOptionMonoid[A]: Monoid[Option[A]] = optionMonoid[A]
  ///def lastOptionMonoid[A]: Monoid[Option[A]] = dual(firstOptionMonoid)


///  A function having the same argument and return type is sometimes called an endofunction
  // Exercise 3
   def endoMonoid[A] = new Monoid[A => A] {
   def op (f: A => A, g: A => A): A => A = f compose g
   val zero: A => A = (a: A) => a
   }

  // Exercise 4 is solved in MonoidSpec.scala

  def concatenate[A] (as: List[A], m: Monoid[A]): A =
    as.foldLeft (m.zero) (m.op)

  // Exercise 7
  //
  // Implement a productMonoid that builds a monoid out of two monoids. Test it
  // with scala check for instance by composing an Option[Int] monoid with a
  // List[String] monoid and running through our monoid laws.

   def productMonoid[A,B] (ma: Monoid[A]) (mb: Monoid[B]) = new Monoid[(A,B)] {
     def op(ab1: (A,B), ab2: (A,B)) : (A,B) = (ma.op(ab1._1 , ab2._1), mb.op(ab1._2 , ab2._2))
       val zero = (ma.zero, mb.zero)
   }


///  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
///    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
///
  ///def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    ///as.map(a => f(a)).fold(m.zero)(m.op)

///  Notice that the type of the function that is passed to `foldRight` is `(A, B) => B`,
///  which can be curried to `A => (B => B)`. This is a strong hint that we should use the endofunction monoid
///   `B => B` to implement `foldRight`. The implementation of `foldLeft` is then just the dual. Don't worry if
///  these implementations are not very efficient.

  /// The function type `(A, B) => B`, when curried, is `A => (B => B)`.
  /// And of course, `B => B` is a monoid for any `B` (via function composition).
  ///def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    ///foldMap(as, endoMonoid[B])(f.curried)(z)

  /// Folding to the left is the same except we flip the arguments to
  /// the function `f` to put the `B` on the correct side.
  /// Then we have to also "flip" the monoid so that it operates from left to right.
  ///def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    ///foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)


}



trait Foldable[F[_]] {

  def foldRight[A,B] (as: F[A]) (z: B) (f: (A,B) => B): B
  def foldLeft[A,B] (as: F[A]) (z: B) (f: (B,A) => B): B
  def foldMap[A,B] (as: F[A]) (f: A => B) (mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  // Exercise 9 (CB 10.15)

   def toList[A] (fa: F[A]) :List[A] =
//     foldRight(fa)(List[A]())(_ :: _)
     foldRight(fa) (List[A]()) ((a: A, b: List[A]) => a +: b )
}

// Exercise 8 (CB 10.12 We just do Foldable[List])

object Foldable extends Foldable[List] {

  def foldRight[A,B] (as: List[A]) (b: B) (f: (A,B) => B): B =
    as.foldRight(b)(f)

  def foldLeft[A,B] (as: List[A]) (b: B) (f: (B,A) => B): B =
    as.foldLeft(b)(f)
  
  def foldMap[A,B] (as: List[A]) (f: A => B) (mb: Monoid[B]): B =
    as.map(f).fold(mb.zero)(mb.op)
}

// vim:cc=80:tw=80
