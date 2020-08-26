// Advanced Programming, Andrzej Wasowski
// Probabilistic Programming (AKA Probability is also a monad)

package adpro

import com.cra.figaro.language.{Constant, Element, Flip, Select, Universe}
import com.cra.figaro.library.compound._
import com.cra.figaro.library.atomic.continuous.{AtomicBeta, Beta}
import com.cra.figaro.library.atomic.discrete.{Binomial, Uniform}
import com.cra.figaro.algorithm.ProbQueryAlgorithm
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.algorithm.factored.VariableElimination
import javax.swing.Box
import shapeless.the

import scala.collection.Map

object BasicProbability {
  //Element is the same as sample or distribution
  val die:  Element[Int] = Uniform[Int] (1,2,3,4,5,6)
  /*A uniform distribution is one in which all values are equally likely within a range (and impossible beyond that range).
    For example, in a uniform distribution from 0 to 10, values from 0 to 1 have a 10% probability as do values from 5 to 6.*/

  // This is like Gen (the only thing is that the API wraps everything in lists
  // because the function is actually variadic, and each sample can contain more
  // than one value --- one for each distribution given as an argument.
  //
  // Importance.sampleJointPosterior (die) take 30 toList
  // generate a random sample from joint posterior distribution (and hence the marginal posterior distributions)
  // Slide 6

  // This is a monad, so we can map! Let's what is the probability that n is
  // prime
  val prime: Element[Boolean] = die map { n => n == 2 || n == 3 || n == 5 }

  // This is a monad, so we can use for notation as well
  val odd: Element[Boolean] = for { n <- die } yield n % 2 == 1

  // Importance generates a random sample like rolling the die

  // scala> Importance.probability (prime, true)
  // res0: Double = 0.5113000000000223
  //
  // scala> Importance.probability (odd,true)
  // res1: Double = 0.4942000000000171
  // Question
  // scala> VariableElimination.probability (prime,true)
  // res2: Double = 0.5
  //
  // scala> Importance.probability (^^(prime,odd), ( (t: (Boolean,Boolean)) => t._1 && t._2 ))
  // res14: Double = 0.33500000000000774



  // prime.observe (true)   //it means event prime be true is occurred

  // scala> Importance.probability (odd,true)
  // res17: Double = 0.6664000000000052
  // changed!!
  // impure

  sealed trait Child
  case object Boy  extends Child
  case object Girl extends Child

  val S = for {
    f <- Uniform(Boy,Girl)
    s <- Uniform(Boy,Girl)
  } yield (f,s)
// val S = ^^(Uniform(BOy,Girl) , Uniform(BOy,Girl))  //^ is read as caret

  val E = S map { case (Boy,Boy) => true; case _ => false }
  val F = S map { case (f, s) => f == Boy || s == Boy }
 //Let F be the event that they have at least one boy, F = {BB, BG, GB}
  Importance.probability (E, true)
//  res0: Double = 0.2546000000000053

  Importance.probability (F, true)
//  res1: Double = 0.746099999999998


  F.observe (true) // yuck!

  Importance.probability (E,true)
//  res2: Double = 0.33310000000000745

 /* E.observe (true)
     Importance.probability (F,true)
     res4: Double = 1.0

     Importance.probability (F,false)
     res5: Double = 0.0

*/

  // Example with Balls

 /* We have two boxes A and B:
  • Box A contains 2 green balls and 7 red balls.
  • Box B contains 4 green balls and 3 red balls.
  Bob selects a ball by
  • first choosing one of the two boxes at random, and
  • then selects one of the balls in this box at random.
    If Bob has selected a red ball, what is the probability that he
    selected a ball from the first box?*/

  val boxA : Element[Boolean] = Flip (.5)  // The ame as Uniform(true,false)  // He could name it as bernoli trail
  //  true if box A, otherwise box B

  val balls = for {
      box <- boxA
      ball <- if (box) Flip (2.0/9)  else Flip (4.0/7) // true if green
    } yield (ball)

  balls.observe (false)
  /*
  He calculated it in opposite direction
  val myboxA : Element[Boolean] = Flip (.5)

  val myballs = for {
      box <- boxA
            ball <- if (box) Flip (7.0/9)  else Flip (3.0/7) // true if red
      } yield (ball)

      myballs.observe (true)

      Importance.probability (myboxA,true)
      VariableElimination.probability(myboxA,true) // more accurate

   */

  // Importance.probability (boxA,true)
  //
  // Expectation

  val X = Uniform (1,2,3,4,5,6)

  // scala> VariableElimination (X)
  // res1: com.cra.figaro.algorithm.factored.ProbQueryVariableElimination = com.cra.figaro.algorithm.factored.ProbQ
  // ueryVariableElimination@6000e77f
  //
  // scala> res1.start
  //
  // scala> res1.expectation (X) (identity)
  // res3: Double = 3.4999999999999996
  //
  // scala> res1.stop
  //
  // scala> res1.kill

}

