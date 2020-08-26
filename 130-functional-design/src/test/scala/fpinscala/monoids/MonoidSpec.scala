// Advanced Programming 2015
// Andrzej Wasowski, IT University of Copenhagen
// Example solution for scala exercises using scalacheck
// Scalacheck's user guide:
// https://github.com/rickynils/scalacheck/wiki/User-Guide

package fpinscala.monoids
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary
import javax.jws.soap.SOAPBinding.Use

object MonoidSpec extends Properties("Monoids..") {

  import Monoid._

  // Exercise 4 (intro to the exercise)

  def associative[A: Arbitrary](m: Monoid[A]): Prop =
    forAll { (a1: A, a2: A, a3: A) =>
      m.op(m.op(a1, a2), a3) == m.op(a1, m.op(a2, a3))
    } :| "associativity"

  def unit[A: Arbitrary](m: Monoid[A]) =
    forAll { (a: A) => m.op(a, m.zero) == a } :| "right unit" &&
      forAll { (a: A) => m.op(m.zero, a) == a } :| "left unit"

  def monoid[A: Arbitrary](m: Monoid[A]): Prop = associative(m) && unit(m)

  property("stringMonoid is a monoid") = monoid(stringMonoid)

  // Exercise 4: test intAddition, intMultiplication, booleanOr,
  // booleanAnd and optionMonoid.

  property("intAddition is a monoid") = monoid(intAddition)
  property("intMultiplication is a monoid") = monoid(intMultiplication)
  property("booleanOr is a monoid") = monoid(booleanOr)
  property("booleanAnd is a monoid") = monoid(booleanAnd)
  property ("optionMonoid is a monoid") = monoid (optionMonoid[Int])

  // Exercise 5
  //Write scalacheck tests that test whether a function is a homomorphism between two sets (monoids). p182
  ///M.op(f(x), f(y)) == f(N.op(x, y)) p 182
  def homomorphism[A: Arbitrary, B: Arbitrary]
  (ma: Monoid[A])(f: A => B)(mb: Monoid[B]) =
    forAll { (a1: A, a2: A) => mb.op(f(a1), f(a2)) == f(ma.op(a1, a2)) }


  ///    property ("---homomorphism") = homomorphism (stringMonoid) (s => s.length) (intAddition)


  /// Use them to test that String and List[Char] are isomorphic.

/// This is only for testing that String and List[Char] are isomorphic via toList(f) and mkString(g)
/// (f andThen g) (a) == a && (g andThen f) (b) == b


///  def SLisomorphism[S: Arbitrary,  L: Arbitrary]
///  (ma: Monoid[S]) (mb: Monoid[L]) (tolist: S => L)(mkstring: L => S) = {
///    forAll { (s: S, l: L) =>
///      (tolist andThen mkstring) (ma.op(s,s)) == (ma.op(s,s)) &&
///        (mkstring andThen tolist) (mb.op(l,l)) == (mb.op(l,l))
///    }
///      //(f andThen g) (a) == a && (g andThen f) (b) == b }
///  }

  ///   property ("stringMonoid and listMonoid[Char] are isomorphic") =
  ///     SLisomorphism(stringMonoid)(listMonoid[Char])((ttolist: String) => ttolist.toList) (_.mkString)


  // Exercise 6

  /// x && y == !((!x)||(!y))
  /// x || y == !((!x)&&(!y))

  /// This is only for testing that booleanOr and booleanAnd are isomorphic via the negation function ( ! )

///  def Booisomorphism[B1: Arbitrary,  B2: Arbitrary]
///  (ma: Monoid[B1]) (mb: Monoid[B2]) (neg: B1 => B2)(negAgain: B2 => B1) = {
///    forAll { (s: B1, l: B2) =>
///      (neg andThen negAgain) (ma.op(s,s)) == (ma.op(s,s)) &&
///        (negAgain andThen neg) (mb.op(l,l)) == (mb.op(l,l))
///    }
///  }

///  property ("booleanOr and booleanAnd are isomorphic") =
///    Booisomorphism(booleanOr) (booleanAnd) ((bOr: Boolean) => ! bOr)   ((bAnd: Boolean) => ! bAnd)

  /// Note the in this case f and f are the same function namely the negation function ( ! ) then you do not need two arbitrary Boolean
//  def Bisomorphism[B: Arbitrary]
//  (ma: Monoid[B]) (mb: Monoid[B]) (neg: B => B)(negAgain: B => B) = {
//    forAll { (s: B, l: B) =>
//      (neg andThen negAgain) (ma.op(s,s)) == (ma.op(s,s)) &&
//        (negAgain andThen neg) (mb.op(l,l)) == (mb.op(l,l))
//    }
//  }

///  property ("booleanOr and booleanAnd are isomorphic") =
///    Bisomorphism(booleanOr) (booleanAnd) ((bOr: Boolean) => ! bOr)   ((bAnd: Boolean) => ! bAnd)

  /// Why not generalize them to generic? We should avoid repeating ourselves


//Generate arbitrary function but to test if two monoid are  isomorphism you need specific functions not arbitrary
//    def isomorphism[A: Arbitrary,  B: Arbitrary]
//    (ma: Monoid[A]) (mb: Monoid[B])  = {
//      forAll { (f: A => B, g: B => A) =>
//      forAll { (a: A, b: B) =>
//        (f andThen g) (ma.op(a,a)) == (ma.op(a,a)) &&
//          (g andThen f) (mb.op(b,b)) == (mb.op(b,b))
//      } }
//    }

  def isomorphism[A: Arbitrary,  B: Arbitrary]
  (ma: Monoid[A]) (mb: Monoid[B]) (f: A => B)( g: B => A)  = {
    forAll { (a: A, b: B) =>
      (f andThen g) (ma.op(a,a)) == (ma.op(a,a)) &&
        (g andThen f) (mb.op(b,b)) == (mb.op(b,b))
    }
  }
    property ("stringMonoid and listMonoid[Char] are isomorphic") =
      isomorphism(stringMonoid)(listMonoid[Char])((tolist: String) => tolist.toList) (_.mkString)

    property ("booleanOr and booleanAnd are isomorphic") =
      isomorphism(booleanOr) (booleanAnd) ((bOr: Boolean) => ! bOr)   ((bAnd: Boolean) => ! bAnd)


  // Exercise 7 (the testing part)

   property ("productMonoid is a monoid") = monoid(productMonoid(intMultiplication)( booleanOr))
}
