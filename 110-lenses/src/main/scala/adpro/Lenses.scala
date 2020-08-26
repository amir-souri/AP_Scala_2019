// Advanced Programming
// Andrzej Wasowski, IT University of Copenhagen
// Exercises/Miniproject/Tutorial on lenses.
//
// Monocle is a library providing lenses for Scala (one of many in fact)
//
// Tutorial for Monocle lenses is here:
// http://julien-truffaut.github.io/Monocle/
//
// We will reimplement some Lenses today, but we shall reuse some basic
// infrastructure from Monocle.  Monocle is *probably* the most popular Lens
// framework for scala, although scalaz provides its own lenses, and so does
// shapeless.
//
// Documentation is sparse so far, but the source code is here:
// https://github.com/julien-truffaut/Monocle
//
// And some examples are here (and in other files in the same directory):
// https://github.com/julien-truffaut/Monocle/blob/master/example/src/test/scala/monocle/LensExample.scala
// We are now advanced enough in Scala and functional programming to understand
// a lot of it :)
//
// Work through the file below in the order of numbered exercises (top to bottom),
// referring to LensesSpec.scala whenever necessary.
//
// Some notes:
//
// 1. Put is called Set in Monocle (and virtually all lens implementations)
//
// 2. Put/Set is curried ([Morries 2012] has a discussion that touches on
//    advantages of currying set)
//
// 3. Total lenses are called Lens in monocle.  Partial lenses are of type
//    Optional.

/*
     Monomoprhic lenses:
     Iso          S => A               A => S                     Isomorphism in math       symmetric
     Prism        S => Option[A]       A => S                     when we have sum types
     Lens         S => A               (A,S) => S                 total lens
     Optional     S => Option[A]       (A,S) => S                 partial lens              compose with both Lens and prism

     Polymorphic lenses:
     PLens[S, T, A, B](get: S => A, set: B => S => T)

     Lens[C,A] (get = C => A) (set = A => C => A)
 */

package adpro

import scalaz._
import monocle.{Iso, Lens, Optional, Prism}
import monocle.macros.GenLens
import monocle.PLens._
import monocle.std.map._
import monocle.syntax._
import monocle.function.all._
import monocle.std.set
import scalaz.Alpha.A

object Lenses {

  // Exercise 0. Study the implementation of lens l1 below and compare it to the
  // first example in Foster et al. (Page 6).
  ///An example of a lens satisfying PutGet but not GetPut is the following.
  ///Suppose C = string × int and A = string, and define l by:
  /// get (s, n) = s       put/set (s' , (s, n)) = (s' , 0)
  /// set(value / abstract / index)(state/ concrete / array ) => (state/ concrete / array )
  /// get(state/ concrete / array ) => (value / abstract / index)

  val l1 = Lens[(String,Int), String] (_._1) (s1 => _ => (s1,0))  //(get) (set)	 // Lens[get, set]  Lens[R, F]  Lens[S, A]

  //val l1 = Lens[(String,Int), String] ( (tup: (String, Int)/*Tuple2[String, Int]*/) => tup._1) ((s1: String) => (tup: (String, Int)) => (s1,0))

  ///Then set (put (s, 1), (s, 1)) = (s, 0) != (s, 1). Intuitively, the law fails because the
  ///putback function has “side effects”: it modifies information in the concrete view
  ///that is not reflected in the abstract view.

  // Complete the second example from page 6, and the example from page 7 below:

  // page 6 in Foster et al.:
  ///An example of a lens satisfying GetPut but not PutGet is the following. Let
  ///C = string and A = string × int, and define l by :
  ///  get s = (s, 0)
  ///  set ((s' , n), s) = s'
  
  val l2  : Lens[String, (String,Int)] =
    Lens[String, (String,Int)] ( (s: String) => (s, 0) ) ( (tup: (String, Int)) => (s: String) => tup._1)
//  Lens[String, (String,Int)]((_, 0)) (s1 => _ => s1._1)

  /// Then get ( set ((s' , 1), s)) = get s' = (s' , 0) != (s' , 1)
  ///PutGet fails here because some information contained in the abstract view does
  ///not get propagated to the new concrete view.

  // page 7 in Foster et al.

  ///We may also consider an optional third law, called PutPut:
  /// set (a' , set (a, c)) != set (a' , c) for all a, a' ∈ A and c ∈ C


  ///For now, a simple example of a lens that is well behaved but not very well
  ///  behaved is as follows. Consider the following lens, where C = string × int and
  /// A = string. The second component of each concrete view intuitively represents a
  ///  version number.

  ///get (s, n) = s
  ///set (s, (s' , n)) = {if s == s' ====> (s, n)
  //                      if s != s' ====> (s, n+1)}

   val l3 : Lens[(String,Int), String] =
//     Lens[(String,Int), String](_._1)(new_val => prev_tuple => {
//     if (new_val == prev_tuple._1) {
//       prev_tuple
//     } else {
//       (new_val, prev_tuple._2 + 1)
//     }
//   })
     Lens[(String,Int), String] ((tup : (String, Int) )=> tup._1 ) ((s: String) => (tup: (String, Int)) =>
       if (s == tup._1) (s, tup._2) else (s, tup._2 + 1))
  ///This lens satisfies both GetPut and PutGet but not PutPut, as we have
  ///set (s, set (s' , (c, n))) = (s, n + 2) != (s, n + 1) = set (s, (c, n)).


  // Exercise 1: Write PutGet law as a property test for arbitrary lenses from
  // type C to typ A; do the same for GetPut and PutPut.  Test the above three
  // lenses and find in the paper whether the results are consistent. Put the
  // tests in LensesSpec.scala in the tests directory.

  // In general it may be difficult to prove mathematically that your lenses
  // obey the good laws, but ... we can cheat :)  There is always property testing!
  // With some plumbing we can even use the implementations of the tests
  // provided by the framework (See the bottom of LensesSpec.scala... but in
  // this exercise please implement your tests from scratch).








  ///////////////////////////////////wright Lenses for exam////////////////////////////////////////////////////////

//   Allways _ not t in set
  def nullOption[T] = Lens[T,Option[T]] (get = (c: T) => if (c == null) None else Some(c)) (
    set = ot => _ /*t*/ => ot match {//getOrElse(null.asInstanceOf[T])
      case Some(v) => v
      case None => null.asInstanceOf[T]
    }
  )

//   Allways _ not e in set
  def eitherOption[A,B] (default: => A): Lens[Either[A,B],Option[B]] =
    Lens[Either[A,B],Option[B]] (get = e => e match {case Left(_) => None
    case Right(v) => Some(v)}) (set = ob => _ /*e*/ => ob match {
      case None =>  Left(default)
      case Some(v) => Right(v)
    })




  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////












  // Exercise 2: Implement the lens codiag from Either[A.A] to A (this is
  // presented by [Morris, 2012]. This may be thought of as taking either A or A
  // and stripping the choice of A from the Either value. The type of this value
  // is Lens[Either[A, A], A].
  //
  // Monocle prefers to use A \/ B instead of Either[A,B].  This is practically
  // the same thing (\/ is introduced by scalaz, and Either comes in the
  // standard library). "-\/" is the name of the left constructor and "\/-" is
  // the name of the right constructor (you can pattern match against them). We
  // use the infix constructor in this exercise, instead of Either.  All the
  // imports in the project are already set up.

   def codiag[A]: Lens[A \/ A, A] =
     Lens[A \/ A, A] (_ match {
       case -\/(l) => l
       case \/-(r) => r
     }) ((a: A) => (e: A \/ A ) => e match {
       case -\/(l) => -\/(a)
       case \/-(r)  => \/- (a)
     })
     //     val id = Lens[A, A](identity) ((a: A) => a => a)



  // Some codiag tests are found in LensesSpec.  Test your solution.

  // Exercise 3: Section 5.3 of [Morris  2012] describes a choice combinator for
  // lenseis |||: Lens[R, F] => Lens[S, F] => Lens[Either[R, S], F].
  //
  // Morris uses it to implement the above codiag lens together with an
  // identity lens (Identity is described in [Foster et al. p 12] and in
  // [Morris p.3 Listing 12].
  //
  // In Monocle '|||' is called "lensChoice.choice" and identity is called
  // "Lens.id", the same way as Morris calls it. Observe also that Monocle's
  // setters are curried, while Morris's are not.
  //
  // Translate morris' implementation of codiag to Monocle and test it.

   def codiag1[A]: Lens[A \/ A, A] =
     lensChoice.choice(Lens.id, Lens.id)

  val id1 = Lens[String, String](identity[String])  ( a => a1 => a)


  //  def codiag4[A]: Lens[A \/ A, A] = {
//    val id1 = Lens[A, A](identity)  ( _ => a1 => a1) //( a => _  => a)
//    lensChoice.choice(id1, id1)
//  }


//     val id = Lens[A, A](identity) ((a: A) => a => a)
//     val test: Lens[A \/ A, A] = lensChoice.choice(Lens.id, Lens.id)
//     test
  // Test this implementation uncommenting tests in LensesSpec.scala


  // Exercise 4: (Important: this exercise shows the main application of lenses)
  //
  // Consider the following types:

  type ZipCode = String
  type Name = String
  type Students = Map[Name, Address]
  case class Address (val zipcode: ZipCode, val country: String)
  case class University (val students: Students, val address: Address)

  // and an example data value:

  val itu = University ( Map(
    "Stefan"    -> Address ("2300",   "Denmark"),
    "Axel"      -> Address ("91000",  "France"),
    "Alex"      -> Address ("2800",   "Denmark"),
    "Christian" -> Address ("D-4242", "Germany"),
    "Andrzej"   -> Address ("00-950", "Poland"),
    "Thorsten"  -> Address ("6767",   "Sweden")
    ), Address("2300", "Amager")
  )

  // Write an expression that modifies "itu" in such a way that Alex is in
  // Denmark but at post-code 9100. First without using lenses.
  //
  // Hint: every class in Scala has a method called 'copy' that takes the same
  // parameters as the constructor.  All parameters are optional.  Use the name
  // assignment convention to only change values of properties that you want in
  // the copy.  For instance itu.copy (students = itu.students.tail) creates a
  // copy of ITU without the first student.


//  val itu1 = itu.copy( students = Map ("Alex" -> Address("9100", "Denmark") ))
val itu1: University = itu.copy( itu.students.updated("Alex", Address("9100", "Denmark"))  )
//("Alex" -> itu.students("Alex").copy( zipcode = "9100" ))

  // There is a test in LensesSpec to check whether  you did what expected.
  //
  // As you see doing this without lenses is very very annoying.  Updating
  // nested properties in complex objects is much easier in imperative
  // programming.



  // Exercise 5.  Lenses to the rescue.  Try to extend our hypothetical
  // university library with lenses, so that using the types is almost as
  // natural as in imperative languages.
  //
  // a) design a lens that accesses zipcode from Address objects:

   val _zipcode: Lens[Address, ZipCode] =
     Lens[Address, ZipCode] (get = _.zipcode)  (set = (z: ZipCode) => (ad: Address) => ad.copy(zipcode = z))

  // b) design a lense that accesses the students collection from university:

   val _students: Lens[University, Students] =
  Lens[University, Students] (get = _.students) (set = (stu: Students) => (uni: University) => uni.copy(stu))

  // c) Use the following index lens (name)  from Monocle:
  //
  // index(name) :Optional[Map[0.String,Address],Address]
  //
  // This lens focuses our view on the entry in a map with a given index.
  // Optional in the Monocle terminology is a partial lens in the terminology
  // of Foster et al.
  //
  // Use lenses composition to update itu the same way as above but in a clearer
  // way (use the infix binary operator ^|-? to compose a lens with an
  // optional, and use ^|-> to compose the optional with a lens).

   val itu2 :University = {
//     val test = _students ^|-? index("Alex") ^|-> _zipcode
//     val un = test.set("9100")(itu)
//     un
     (_students ^|-? index("Alex") ^|-> _zipcode).set("9100")(itu)
//     (_students composeOptional index("Alex") composeLens _zipcode).set("9100")(itu)
   }
  // There is a test in LensesSpec to test whether what you have built behaves
  // as expected.
  //
  // Now once you provide lenses for your types, navigating and modifying deep
  // structures becomes more readable and easier to write.  In fact, lense
  // libraries provide various mechanisms to generate them for the properties of
  // your case classes, so this access can come at almost no (coding) cost.


  // Exercise 6. We shall now turn upper case names of all countries in all the
  // addresses of all students in the itu object.
  //
  // We shall use the 'modify' function of lenses. Morris describes modify
  // problem in Section 2, and shows the lens solution in Listing 9.  Monocle
  // has a modify method in Lens[A.B]:
  //
  //    modify : (B => B) => A => A
  //
  // It works almost like get and set at the same time (so you use modify if you
  // would otherwise like to get a value, and then make a modification to this
  // value).  Modify takes a function that makes the change (computes the new
  // data) and then the source(concrete) object.  It returns the new object. It
  // is potentially more efficient than using get and set separately.
  //Question:
  // In this exercise we will use modify to perform a cross cutting modification
  // on a complex structure.
  //
  // We will need a lens that gives us all countries from the map of students.
  // This kind of lens is called a Traversable in Monocle.
  //
  // We use infix ^|->> to compose an optical (Lens, Traversable, Optional, etc)
  // with a traversable (as we use ^|-> to compose any of these with a Lens).
  //
  // The traversable "each" (which has a default instance for maps) will give us
  // a collection of all objects (values in a map).  So to solve the task we
  // need to compose:
  //
  // - a lens that extracts the students collection from a University
  // (_students)
  //
  // - a traversable that extracts all objects from a collection (each)
  //
  // - a lens that extract the country from an address object (_country, you
  // will need to write that one, as we did not create it yet).

    val _country :Lens[Address,String] =
      Lens[Address,String] ((ad: Address) => ad.country /*_.country*/)  ((cou: String) => (ad: Address) => ad.copy(country = cou))



    val itu3 :University = {
//      val test = _students ^|->> each ^|-> _country
//      val un = test.modify(s => s.toUpperCase()) (itu)
//      un
      (_students ^|->> each ^|-> _country).modify(s => s.toUpperCase()) (itu)
//      (_students composeTraversal each composeLens _country).modify(v => v.toUpperCase)(itu)
    }

  // LensesSpec.scala has a test to see if you succeeded.
  //
  // QUESTION: Compare the test with the code used above.  Why have we used
  // lenses/traversals above, and not in the test? What is the difference
  // between the code in the test and the code above that influences this? Write
  // the answer below:
  //
  // ... ... ...




  // Exercise 7. Use filterIndex(p) to only capitalize city names of the
  // students on the list whose name satisfies predicate (p). Let's capitalize
  // the names of students whose name begins with letter A.  The filterIndex
  // combinator is a traversal, like 'each' above. Recall that ^|->> is used to
  // compose (append) a traversal and ^|-> is used to append a lens.

   val itu4 = {
//     val test = _students ^|->> filterIndex((name: String) => name.startsWith("A")) ^|-> _country
//     val un = test.modify(s => s.toUpperCase()) (itu)
//     un
     (_students ^|->> filterIndex((name: String) => name.startsWith("A")) ^|-> _country).modify(s => s.toUpperCase()) (itu)
//     (_students composeTraversal filterIndex{ name: String => name(0) == 'A' } composeLens _country).modify(v => {println(v); v.toUpperCase})(itu)
   }

//   println (itu4) [cheap testing]


  // Exercise 8.  We are returning to construction of basic lenses.  Implement a
  // (partial) lens that accesses ith element of a list (let's call it index).
  // A partial lens (Prism), so a Optional in Monocle terminology, would be of type
  // Optional[List[A],A].  The Optional takes two parameters for the
  // constructor:
  //
  // get: List[A] => Option[A]
  // set: A => List[A] => List[A]
  //
   def setIth[A] (n: Integer) :Optional[List[A],A] =
//    List(1,2,3,4,5).lift(2)
//  res42: Option[Int] = Some(3)

  Optional[List[A],A] (/*l => l.lift(n)*/_.lift(n))  (a => (la : List[A]) => la.updated(n,a))
  //    Optional[List[A],A] (l => l.lift(n)/*_.lift(n)*/)  (a => (la : List[A]) => if (n < la.length) la.updated(n, a) else la )
//  if (n < la.length) la.updated(n, a) else la



  // In the above you will need to decide what to do with the setter if n is
  // greater than the length of the list.  One option is to do nothing, just
  // ignore the setting :).  Another alternative is to provide a default
  // element, and extend the list appropriately. In such case we obtain a total
  // lens. Try this too:

   def setIth1[A] (n: Integer, default: A) :Lens[List[A],A] =  //.. TODO ca. 12 lines

     Lens[List[A],A](
       (l: List[A]) => if (n < l.length) l(n) else default
     )(
       (n_elm: A) => (l: List[A]) => {
         if (n < l.length)
           l.updated(n, n_elm)
         else {
           l ++ List.fill(n - l.length)(default) :+ n_elm
         }
       }
     )




//     Lens[List[A],A] (get = la => {
//      if (!la.isEmpty && la.length > n ) la(n)
//       else default
//     }
//     )   (a => (la: List[A]) =>
//       if (la.length > n  && (!la.isEmpty)) {
////       val t = la.take(n)
////       val d = la.drop(n)
//        val (f,s) = (la.splitAt(n))
//
//       f ::: List(a) ::: s.tail  
///  ::: == ++  / List(1) ::: List(2) == List(1) ++ List(2)
/// List(1,2,3) :+ 4 == List(1,2,3,4) / 4 +: List(1,2,3) == List(4,1,2,3)
/// 4 :: List(1,2,3) == List(4,1,2,3) 
/// List(1,2) :: List(300,400) == List(List(1, 2), 300, 400) 
///
//       }
//       else if (la.isEmpty) {
//         List.fill(n)(default) ::: List(a)
//
//       }
//       else {
//         val l = la.length
//         val l_new = n - l
//         la ::: List.fill(l_new)(default) ::: List(a)
//       }
//
//     )

























//     Optional[List[A],A] (_getOption = la => Option(la(n)) ) (_set =
//       (a) => (la: List[A]) => {
//         if ( n <= la.length - 1)
//         la.patch(n, Seq(a), 1)
//         else
//         for (1 to n)
//       }

  //)

   //}



  // Exercise 9. To test setIth (above) you will also need to implement new
  // PutGet, GetPut and PutPut laws that work for Optionals. Add the tests to
  // LensesSpec.scala.  Carefully consider the equality of results as used in
  // Foster et al. (The \sqsubseteq ordering)
  //
  // Test setIth1 with existing laws that you have already used for your earlier
  // lenses (setIth1 is a usual total lens).
  //
  // Proceed in LensesSpec.scala


  // Exercise 10.  setIth demonstrates that lenses emulate a form of
  // "imperative" programming, by making a structure updatedable, even deeply.
  // For a simple example, use sethIth below to increment the third element on a
  // list list0

//   val list0 = List[Int](1,2,3,4,5,6)
//   val list1 = setIth(2).modify((i: Int) => i + 1)(list0)
//  setIth[Int](2).set(setIth[Int](2).getOption(list0).get + 1 ) (list0)

//   println (list0)
//   println (list1)

}




//
//Optional[List[A],A] (_getOption = la => la match {
////case Nil => None
//case h :: t if (la.length > n ) => Some(la(n))
////case Nil => Some( )
//case _ => None
////Option[A](la(n))
//}
//) (_set = (i) =>
//(la: List[A]) => {
////la.patch(n, Seq(i), 1)
//if (la.length > n  && (!la.isEmpty)) {
//val t = la.take(n)
//val d = la.drop(n)
//t ::: List(i) ::: d
//}
//else if (la.isEmpty) List()
//else la
//}
//
//)
