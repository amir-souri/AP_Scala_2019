//Group number: 17
//AUTHOR1: Amir Souri asou@itu.dk
//AUTHOR2: Filip Dusek  fidu@itu.dk
//AUTHOR3: Abdullah Al Maruf  abma@itu.dk
package fpinscala.laziness
import scala.language.higherKinds
import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary


// If you comment out all the import lines below, then you test the Scala
// Standard Library implementation of Streams. Interestingly, the standard
// library streams are stricter than those from the book, so some laziness tests
// fail on them.

//Note, that if you comment out all imports, you are testing the standard library's
//implementation of Streams (which is not what we want to do).
import stream00._    // uncomment to test the book solution
//import stream01._ // uncomment to test the broken headOption implementation
//import stream02._ // uncomment to test another version that breaks headOption

class StreamSpecAsouAbmaFidu extends FreeSpec with Matchers with PropertyChecks {

  import Stream._

  // A simple converter of lists to streams
  def list2stream[A] (la :List[A]): Stream[A] =
    la.foldRight (Stream.empty[A]) (cons[A](_,_))


  // An example generator of random finite non-empty streams
  // (we use the built in generator of lists and convert them to streams,
  // using the above converter)

  def genNonEmptyStream[A] (implicit arbA :Arbitrary[A]) :Gen[Stream[A]] = 
    for {
      la <- arbitrary[List[A]] suchThat { _.nonEmpty }
    } yield list2stream (la)

  "headOption" - {

    // a scenario test:
    "returns None on an empty Stream (01)" in {
      (Stream.empty.headOption) shouldBe (None)
    }


    // two property tests:

    "returns the head of a singleton stream packaged in Some (02)" in {
      forAll { (n :Int) => cons (n, Stream.empty).headOption shouldBe Some(n) }
    }

    "returns the head of random stream packaged in Some (02)" in {
      // The implicit makes the generator available in the context
      implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])

      // This property uses our generator of non empty streams thanks to the above implicit declaration
      forAll { (s :Stream[Int]) => s.headOption should !== (None) }
    }

    "should not force the tail of the stream" in{
      Cons( ()=> 0, () => {throw new Exception("It forced the tail of the Stream"); Empty}).headOption
    }

    "should not force the tail of the stream 2" in{

     forAll ("n") { (n :Int) => 
     cons(n, cons( n * 2 , cons( 7, {throw new Exception("It forced the tail of the stream") }))).headOption
     }
    }
    

  }

  
  "take" - {

    "take should not force any heads nor any tails of the Stream it manipulates" in {
   
      implicit def arbIntStream = Arbitrary[Stream[Boolean]] (genNonEmptyStream[Boolean])
      forAll ("stream", "n"){
        (s: Stream[Boolean], n: Int) =>
          s.map(a => { fail("It forced the head or the tail of the Stream"); a}).take(n)
      }

    }


    "take(n) does not force (n+1)st head ever (even if we force all elements of take(n))" in {
    val tiredHead = () => {throw new Exception("It forced (n+1) element"); 1}
    cons(tiredHead(), Empty).take(0).toList
    cons(1, cons(tiredHead(), Empty)).take(1).toList
    }


    "s.take(n).take(n) == s.take(n) for any Stream s and any n (idempotency)" in {
      implicit def arbIntStream = Arbitrary[Stream[String]] (genNonEmptyStream[String])
      forAll ("stream", "n"){
        (s: Stream[String], n: Int) =>
        s.take(n).take(n).toList shouldEqual s.take(n).toList

      }
    }


  }


  "drop" - {

    "s.drop(n).drop(m) == s.drop(n+m) for any n, m (additivity)" in{

    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    implicit def arbPositiveInt = Arbitrary[Int](Gen.choose(1, 2147483647 / 2))
    //implicit def arbPositiveInt = Arbitrary[(Int, Int)] ((Gen.choose(1, 20) , Gen.choose(-1, -20)))
    
    /*type TupleInt = (Int,Int)
    implicit def arbTuple : TupleInt= for {
      m <- Gen.choose(1, 1)
      n <- Gen.choose(1, 1)
    } yield (m, n)*/
    
      forAll { (s: Stream[Int], n: Int, m: Int)  => {
         //whenever ( m > 0 && n > 0 ) {
          //val n:Int = t._1
          //val m:Int = t._2
        
          s.drop(n).drop(m).toList shouldEqual s.drop(n+m).toList 
          println(m)
          println(n)

        }
      }
      //}
    }

    "s.drop(n) does not force any of the dropped elements heads" in {
      implicit def arbIntStream = Arbitrary[Stream[Float]] (genNonEmptyStream[Float])
      forAll ("stream", "n"){
        (s: Stream[Float], n: Int) =>
          s.map(a => { throw new Exception("It forced the head of the stream"); a}).drop(n)    

      }
    }


    "the above should hold even if we force some stuff in the tail" in {
      val tiredHead = () => {throw new Exception("It forced the head of the stream when forcing the tail"); 1}
      Cons(tiredHead, () => Cons(() => 1, () => Empty)).drop(1).toList 
    }



  }




  "map" - {

    "x.map(id) == x (where id is the identity function)" in{
      implicit def arbIntStream = Arbitrary[Stream[Double]] (genNonEmptyStream[Double])
      forAll  ("stream")    {
        (s: Stream[Double]) =>
        s.map(a => a).toList shouldEqual s.toList
      }
    }


    "map terminates on infinite streams" in {
      from(0).map(a => a+10)
    }


  }




 "append" - {
  "Stream(1, 2).append(Stream(3, 4)) == Stream(1, 2, 3, 4)" in {
    implicit def arbIntStream = Arbitrary[Stream[Char]] (genNonEmptyStream[Char])
    forAll {
      (s: Stream[Char]) => 
      s.append(Stream(12)).toList shouldEqual s.toList ++ Stream(12).toList
    }
  }


  "Stream(1, 2).append(Empty) == Stream(1, 2) && Empty.append(Stream(1, 2)) == Stream(1, 2)" in {
    assert( Stream(1,2).append(Empty).toList == Stream(1,2).toList &&
    Empty.append(Stream(1,2)).toList == Stream(1,2).toList )
    
  }

  "It nither force the heads nor tails of the streams" in {
    forAll ("n") { (n: Int) =>
    from(n).append (from(n)) //(useless, but should not crash)
    }
  }



 }




}

