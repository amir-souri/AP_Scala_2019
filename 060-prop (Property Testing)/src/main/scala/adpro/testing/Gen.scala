// Advanced Programming, A. Wąsowski, IT University of Copenhagen
//
// Group number: 17
//
// AUTHOR1: Abdullah Al Maruf  abma@itu.dk
// TIME1: 11 <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// AUTHOR2: Filip Dusek  fidu@itu.dk
// TIME2: 11 <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// AUTHOR3: Amir Souri asou@itu.dk
// TIME3: 11 <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// You should work with the file by following the associated exercise sheet
// (available in PDF from the course website).
//
// The file shall always compile and run after you are done with each exercise
// (if you do them in order).  Please compile and test frequently. Of course,
// some tests will be failing until you finish. Only hand in a solution that
// compiles and where tests pass for all parts that you finished.    The tests
// will fail for unfnished parts.  Comment such out.
//
// Before starting to work on the exercises, familiarize yourself with the the
// content already in the file (it has been explained in chapter 8, but it is
// useful to see it all together in one file).

package adpro.testing
import fpinscala.state._
import fpinscala.state.RNG._
import adpro.testing.Prop.Passed
import adpro.testing.Prop.Falsified
import adpro.testing.Prop.Proved

// A generator will use a random number generator RNG in its state, to create
// random instances (but perhaps also some other staff)
case class Gen[A] (sample :State[RNG,A]) {

  // Let's convert generator to streams of generators
  def toStream (seed: Long): Stream[A] =
    Gen.state2stream (this.sample) (RNG.Simple (seed))
  def toStream (rng: RNG): Stream[A] =
    Gen.state2stream (this.sample) (rng)

  // Exercise 3
  //def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
  //def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] Generates lists of length n using the generator g
  //this.sample returns State[RNG,A]

  def listOfN (n: Int): Gen[List[A]] = {
    //def fill[A] (n: Int) (elem: =>A): List[A]
  Gen(State.sequence(List.fill(n)(this.sample)))
    
  }

  //listOfN(7)

  // Exercise 4
  //recall flatMap in State:
  //def flatMap[B](f: A => State[S, B]): State[S, B] =

  // It takes a function from A to State[S, B]. But Gen.flatMap takes a function from A to Gen[B].
  //Therfore we cannot pass f of Gen.flatMap in State.flatMap. We know Gen has a sample which returns 
  //State[RNG,A] and f returns Gen[B], so if we call sample on it the resualt is a State[RNG,A] wich is in line
  //with the requared return type of f in State.flatMap


  def flatMap[B] (f: A => Gen[B]): Gen[B] = 
  Gen(   this.sample.flatMap(  f(_).sample    )  )

  // It would be convenient to also have map  (uses flatMap)

  def map[B] (f: A => B): Gen[B] = this.flatMap (a => Gen.unit[B] (f(a)))

  // Exercise 5

  def listOfN (size: Gen[Int]): Gen[List[A]] ={
  size.flatMap(     n => Gen(State.sequence(List.fill(n)(this.sample)))      )      
  //size flatMap (n => this.listOfN(n))

  }
  //(List.fill(size)(this.sample)).flatMap(a => Gen(a))

  // Exercise 6

  def union (that: Gen[A]): Gen[A] =
  Gen.boolean.flatMap( tf => if (tf == true) this else that   )    
  //boolean.flatMap(b => if (tf) this else that)          

  // Exercise 7 continues in the companion object (below)
}

object Gen {

  // A convenience function to convert states (automata) to streams (traces)
  // It would be better to have it in State, but I am not controlling
  // State.scala.

  private  def state2stream[A] (s :State[RNG,A]) (seed :RNG) :Stream[A] =
    s.run(seed) match { case (n,s1) => n #:: state2stream (s) (s1) }

  // A generator for Integer instances

  def anyInteger: Gen[Int] = Gen(State(_.nextInt))

  // Exercise 1

  def choose (start: Int, stopExclusive: Int): Gen[Int] = {
  Gen(State(RNG.nonNegativeInt).map(n => 
  start + n % (stopExclusive-start)
  //In general you can get a number between x and y with help of modulo i.e x % y. But if x < y then x % y 
  //resualt in x. In choos function we have this situation. Even if x was gereater than y, we should not 
  //use x % y since it reesulat in a certain number not a random one between x and y.
  ))}
  //modulo peroperties:
  // Distributive in the first component: no.  5 % (2 + 3) = 0, while (5 % 2) + (5 % 3) = 1 + 2 = 3
  // Distributive in the second component: yes, assuming the implementation handles negative arguments properly 
  //and there's no overflow involved.  (x + y) % n = (x % n) + (y % n)
  // Associative: No.  (3 % 4) % 2 = 3 % 2 = 1, while 3 % (4 % 2) = 3 % 0 which is either 0 or undefined.
  // Commutative: No.  (2 % 4) = 2, while (4 % 2) = 0.

  // Exercise 2
  //recall:
  //def unit[S, A](a: A): State[S, A]
  //State(s => (a, s))

// case class Gen[A] (sample :State[RNG,A])

  def unit[A] (a: =>A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))
  // Gen(State(RNG.nonNegativeInt).map (n => 
  // // if (n % 2 == 0) true else false
  // n % 2 == 0
  // ))

  def double: Gen[Double] = Gen(State(RNG.double))
  //def double: Gen[Double] = Gen(State(rng => RNG.double(rng)))

  //State( rng => RNG.nonNegativeInt(rng) ) == State(RNG.double)

  // (Exercise 3 is found in the Gen class above)

}

// This is the Prop type implemented in [Chiusano, Bjarnasson 2015]

object Prop {

  type TestCases = Int
  type SuccessCount = Int
  type FailedCase = String

  // the type of results returned by property testing

  sealed trait Result { def isFalsified: Boolean }
  case object Passed extends Result { def isFalsified = false }
  case class Falsified(failure: FailedCase,
    successes: SuccessCount) extends Result {
      def isFalsified = true
  }
  case object Proved extends Result { def isFalsified = false }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => as.toStream(rng).zip(Stream.from(0)).take(n).map {
      case (a,i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

import Prop._

case class Prop (run: (TestCases,RNG)=>Result) {

  // (Exercise 7)
  //case Passed case Proved case Falsified
  def && (that: Prop): Prop = Prop {  (n, rng) => run(n, rng) match {
    case Passed => that.run(n, rng)
    case x => x
  }}

  def || (that: Prop): Prop = Prop { (n, rng) => run(n, rng) match {
    case Falsified(msg, _) => that.run(n,rng)
    case x => x
   }}

}

// vim:cc=80:foldmethod=indent:nofoldenable
