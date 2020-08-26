// Advanced Programming, Exercises by A. Wąsowski, IT University of Copenhagen
//
// AUTHOR1:
// AUTHOR2:
//
// Write ITU email addresses of both group members that contributed to
// the solution of the exercise (in lexicographic order).
//
// You should work with the file by following the associated exercise sheet
// (available in PDF from the course website).
//
// The file is meant to be compiled inside sbt, using the 'compile' command.
// To run the compiled file use the 'run' or 'runMain' command.32
// To load the file int the REPL use the 'console' command.
// Now you can interactively experiment with your code.
//
// Continue solving exercises in the order presented in the PDF file. The file
// shall always compile, run, and pass tests, after you are done with each
// exercise (if you do them in order).  Please compile and test frequently.

// The extension of App allows writing the main method statements at the top
// level (the so called default constructor). For App objects they will be
// executed as if they were placed in the main method in Java.

// import fpinscala.Exercises._ do it to use all functions in consoled
package fpinscala

object Exercises extends App with ExercisesInterface {

  import fpinscala.List._

//   def test (a: List[Int]) = a match {
//      case Cons(h, t) => h + 30
//  case Cons(x, Cons(2, Cons(3, _))) => "one"
//  case Nil => 42
//  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => "tr"
//   case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => "fo"
//  case Cons(h, t) => h + 30
//  case _ => 101
//  }

  // Exercise 3

  // add @annotation.tailrec to make the compiler check that your solution is
  // tail recursive
  def fib (n: Int) : Int ={
    @annotation.tailrec
     def go(n: Int, current: Int, next: Int): Int =  {
    if (n <= 0)   { 0 }
     else if (n == 1)    {current}
     else {
       //println(current)
       go(n - 1, current = current + next, next = current)
    }
    }
    go(n, current = 0, next = 1)
  }
    

  //   def fibWithTuples(i: Int): BigInt = {
  //     @annotation.tailrec
  //     def loop(i: Int, acc: (BigInt, BigInt) //tuple// ): BigInt = {
  //       if (i <= 0) 0
  //        else if (i == 1) {
  //         acc._1
  //        } else {
  //           loop(i - 1, (acc._1 + acc._2, acc._1))
  //          }
  //   }
  //   loop(i, (0, 1))
  // }
    
  

  // Exercise 4

  // add @annotation.tailrec to make the compiler check that your solution is
  // tail recursive
  def isSorted[A] (as: Array[A], ordered: (A,A) =>  Boolean) :Boolean = {
  @annotation.tailrec
  def go(n: Int): Boolean =
    if (n + 1 >= as.length ) true
    else if (!ordered(as(n), as(n + 1)))  false
    else go(n + 1)

  go(0)
 }

/* https://blog.ssanj.net/posts/2017-10-29-tuples-are-different-to-function-arguments-in-scala.html
scala> xs
res35: List[Int] = List(1, 2, 3, 4)

scala> xs.zip(xs.tail)
res30: List[(Int, Int)] = List((1,2), (2,3), (3,4)) 

def tupled: ((T1, T2)) => R
Creates a tupled version of this function: instead of 2 arguments, it accepts a single scala.Tuple2 argument. 
returns a function f such that f((x1, x2)) == f(Tuple2(x1, x2)) == apply(x1, x2)
     trait DefaultMap have all transform function which take tuples as argument 

    scala> def sum(n1: Int, n2: Int) = n1 + n2
    sum: (n1: Int, n2: Int)Int

    scala> val t2 = (2, 4)
    t2: (Int, Int) = (2,4)

    scala> sum _ tupled t2
    res72: Int = 6

    1.Convert the sum method into a Function:
    scala> val f1 = sum _

    2.Convert the function into its tupled variant:
    scala> val f2 = f1.tupled


def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
          if (as.length <= 1) true
                else          as.zip(as.tail).forall(ordered.tupled)
    }   */



  // Exercise 5

  def curry[A,B,C] (f: (A,B)=>C): A => (B => C)  = 
  a => b => f(a, b)
  //(a:A)=>((b:B)=>f(a,b))

  // def curry[A, B, C](f: (A, B) => C): A => (B => C) = A => {
  //   val f1: B => C = f(A, _)
  //       f1


//  def isSorted[A] (as: Array[A], ordered: (A,A) =>  Boolean) :Boolean = {
//   @annotation.tailrec
//   def go(n: Int): Boolean =
//     if (n + 1 >= as.length ) true
//     else if (!ordered(as(n), as(n + 1)))  false
//     else go(n + 1)

//   go(0)
//  }


//_Question1      Why Nothing? if we made isSorted type parameter Int instead of A 
// Answer {
// val isSortedCurry = curry(isSorted[String] _)
// isSortedCurry: Array[String] => (((String, String) => Boolean) => Boolean) = $$Lambda$3246/1889076390@6632303e

// def isSortedCurry[A] = curry(isSorted[A] _)
// isSortedCurry: [A]=> Array[A] => (((A, A) => Boolean) => Boolean)
//}

// (def isSortedIntArray (as: Array[Int], ordered: (Int,Int) =>  Boolean) :Boolean ) it would work fine

//scala> val isSortedCurry = curry(isSorted _)
//isSortedCurry: Array[Nothing] => (((Nothing, Nothing) => Boolean) => Boolean) = $$Lambda$3379/1234532350@3e313e95

//scala> curry(isSortedList _)
//res80: List[Nothing] => (((Nothing, Nothing) => Boolean) => Boolean) = $$Lambda$3379/1234532350@3cca3c00


  // Exercise 6
  //f: A => B => C == f(a)(b)
  def uncurry[A,B,C] (f: A => B => C): (A,B) => C = 
  (a, b) => f(a)(b)
  // (a:A,b:B)=>f(a)(b)

  // Exercise 7

  def compose[A,B,C] (f: B => C, g: A => B) : A => C = 
  a => f(g(a))
  //(a:A) => f(g(a))

  // Exercise 8 requires no programming
// def sum(lI: List[Int]) : Int =
// lI.foldRight(0)((x, y) => x + y)

// private def sum(xs: List[Int]): Int = xs match {
// case Nil              => 0
//case head::tail => head + sum(tail)
//}

//  List(1,2,3,4,5) match {
// // case Cons(x, Cons(2, Cons(4, _))) => x
// case x::2::4::_ => x
//  case Nil => 42
// // case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
// case x::y::3::4::_ => x + y
// // case Cons(h, t) => h + sum(t)
// case h::t => h + sum(t)
//  case _ => 101
//  }

  // Exercise 9

  def tail[A] (as: List[A]) :List[A] = as match {
    case Nil => sys.error("Nil")
    case Cons(_, t) => t
  }

  // def tailOption[A](xs: List[A]): Option[List[A]] = {
  //   xs match {
  //     case _::ys => Some(ys)
  //     case Nil         => None
  //   }
  // }

  // Exercise 10

  // @annotation.tailrec
  // Uncommment the annotation after solving to make the
  // compiler check whether you made the solution tail recursive
  def drop[A] (l: List[A], n: Int) : List[A] = {
      if(n==0) l;
      else l match {
        case Nil => sys.error("You have an error")
        case Cons(_,t) => drop(t, n-1)
      }
    }

    

//     def drop2[A](l: List[A], n: Int): List[A] = {
//     @annotation.tailrec
//     def loop(xs: List[A], n: Int): List[A] = xs match{
//    	case Nil => List()
//    	case h::t if (n > 0) => loop(t, n-1)
//   	case _ => xs
//   }

// loop(l, n)
// }



// @annotation.tailrec
// final def drop3[A](l: List[A], n: Int): List[A] = {
// 	(l, n) match {
// 	case (_, i) if i <= 0 => l
// 	case (_::xs, i) => drop3(xs, i - 1)
// 	case (Nil, _)         => Nil
// 	}
//   }

  // Exercise 11
  // Uncommment the annotation after solving to make the
  // compiler check whether you made the solution tail recursive

  // _Question2 val l = List(1,2,3) dropWhile(l, (x: Int) => x < 20) must return Nil. what if l was Nil
  // initially? it would return an error like "Empty list" but if i add case Nil => sys.error("Empty list") it will 
  // return the error even val l = List(1,2,3) dropWhile(l, (x: Int) => x < 20)?
  // Because  the order is matter in pattern matching so when I first check if l is Nil then return sys.error.
  // It will return the error even when none of elements of the passed l does not satisfy the predict function.
  // if it would return Nil it was fine.
   @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    //case Nil => Nil  //sys.error("Empty list")
    case Cons(h,t) if f(h) => dropWhile(t,f) // if f(h) is the same as if (f(h))
    case _ => l // N.B we could define case Nill => List() but 
    //in this function the case _ takes care of case Nill beside return the output
  }

  // Exercise 12

  //_Question3 How to implement function init which is pure and tail recursive?  With Option or Either type
  // @annotation.tailrec   tested not tail recurcive
  def init[A](l: List[A]): List[A] = l match {
    //N.B This solution is not tail recurcive thus stack over flow run time error
    case Nil => sys.error("Empty list")
    case Cons(_,Nil) => Nil   //if the list has only one element
    case Cons(h,t)=> Cons(h,init(t))
  }
//_Question4 is it a pure? No since sys.error("Empty list")
// def init2[A](l: List[A]): List[A] = {
//   //N.B This solution has side effect thus the function is not referential transparency therefore nor pure function
// val re = l.reverse
// l match {
// case Nil => sys.error("Empty list")
// case _::Nil => Nil   //if the list has only one element
// case h::t => re.tail.reverse
// }
// }


  // Exercise 13
//N.B acc = z: B = 0 = initial value
// we could implement it by foldLeft as well. Indeed, foldLeft(as, 0) (acc, _) => acc + 1

  def length[A] (as: List[A]): Int = 
  foldRight(as, 0)((_, acc) => acc + 1)
  //foldLeft(as, 0) ((acc, _) => acc + 1)


  // Exercise 14
 
  // Uncommment the annotation after solving to make the
  // compiler check whether you made the solution tail recursive
     @annotation.tailrec
  def foldLeft[A,B] (as: List[A], z: B) (f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  // def foldLeftMine[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
  //   def loop(l: List[A], z: B, f: (B, A) => B, acc: B): B = l match {
  //     case Nil => acc
  //     case Cons(x, xs) => loop(xs, z, f, f(acc, x))
  //   }
  //   loop(l, z, f, z)
  // }

  // Exercise 15
  def product (as: List[Int]): Int = 
  foldLeft(as, 1)(_*_)

//  def product2[A](as: List[A])(implicit num: Numeric[A]): A = {
//     as.foldLeft(num.one)(num.times(_, _))
//   }

  // One naive implementation = stack overflow
  // def product2 (as: List[Int]): Int = as match {
  //       case Nil => 1
  //       case Cons(0,_) => 0
  //       case Cons(h,t)=> h * product(t)
  //    }

   def length1 (as: List[Int]): Int = 
  //foldRight(as, 0)((_, acc) => acc + 1)
  foldLeft(as, 0) ((acc, _) => acc + 1)


  

  // Exercise 16

  def reverse[A] (as: List[A]): List[A] =
  //foldRight(as, Nil:List[A])((el ,acc) => Cons(acc, el))
  foldLeft(as, Nil:List[A])((acc, el) => Cons(el, acc))
  // myreverse is not tail recursive
  // def myreverse(as: List[Int]): List[Int] = 
  // as match {
  // case Nil => Nil
  // case h::t => myreverse(t)::h       //myreverse(t)++List(h) it works fine
  // }


  // Exercise 17

  
// _Question5 What is the reason for reversing the list meanwhile without reversing, the result is the same?
//when you want to copy the list you will see the other way around if you did not reverse the list in impelementation.
// with foldLeft/foldRight(as, z)((b:B, a:A) => f(a,b))  (without reverse) 
 //import fpinscala._
//val l = List(1,2,3,4,5)
//res14: fpinscala.List[Int] = Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Nil)))))

//  foldRight1[Int,List[Int]] (l,Nil)((x,z) => Cons(x,z))
// res13: fpinscala.List[Int] = Cons(5,Cons(4,Cons(3,Cons(2,Cons(1,Nil)))))

  def foldRight1[A,B] (as: List[A], z: B) (f: (A, B) => B): B = 
  //foldLeft(reverse(as), z)((b:B, a:A) => f(a,b))//it is basically foldRight since we changed the order of arguments in f
  //foldLeft[A, B=>B]  (as, (b: B) => b) ((g,a) => (b: B) => g(f(a,b)))(z)

  as match{
    case Nil => z
    case Cons(h,t) => f(h, foldRight(t, z) (f))
  }

  // Exercise 18
  
  def foldLeft1[A,B] (as: List[A], z: B) (f: (B,A) => B): B = 

    //foldRight(reverse(as), z)((a: A, b: B) => f(b, a)) //it is basically foldLeft since we changed the order of arguments in f
  
    // val foldlef = foldRight[A,B=>B] (as, identity[B]) ((a,g) => b => g(f(b,a))) 
    // foldlef(z)

    as match {
      case Nil => z
      case Cons(h,t) => foldLeft(t, f(z, h)) (f)
    }



  // Exercise 19
 //_Question 6 Why i can pass list of list of type A? How to avoid this?  Becuse A can be Any Type. It can be a List, Amir...
 // To avoid it: bound the type A
 // def why[A <: Int](a: A) = 5
// why: [A <: Int](a: A)Int
//why(5)
// res0: Int = 5      // as expected
//but:
//val l = List(5)
// l: List[Int] = List(5)
// scala> why(l)
// <console>:14: error: inferred type arguments [List[Int]] do not conform to method why's type parameter bounds [A <: Int]
//        why(l)
//        ^
// <console>:14: error: type mismatch;
//  found   : List[Int]
//  required: A
//        why(l)
//            ^
  def append[A ](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2)) //h::append(t, a2)
  }

  def concat[A] (as: List[List[A]]): List[A] = //as match {
    //case Nil => Nil
    //case Cons(h,t) => append(h, concat(t)) //scala> con3(ll) res20: List[A] = List(1, 2, 3, 4, 5) ll= List[List[A]] 
   // To concatenate two List ::: which is works as our append
  // l:::ll res19: List[Any] = List(6, 7, 8, 9, List(1, 2), List(3), List(), List(4, 5)) l = List[A]
  //}
// Solution by foldRight or foldLeft    concatenate is the same as + (associative(شرکت پذیر) and commutative(جابجایی پذیر))
foldRight(as, /*List[A] or List()*/ List[A]())((x,y)=> append(x,y))
//foldLeft (as, Nil: List[A])((x,y)=> append(x,y))

//_Question7  What does List[A] means and difference between it and List()/Nil
//why List() does not compile?  even nil : List[A]  
// Becuse you need value not type and keep it in your mind that it shoud be of type A
//   <pastie>:15: error: type mismatch;
//   found   : List[A]
//     (which expands to)  List[Int]
//   required: List[Nothing]
  

  // Exercise 20

  def filter[A] (as: List[A]) (f: A => Boolean): List[A] = 
  foldRight(as, Nil:List[A]) ((h,t) => if (f(h)) Cons(h,t) else t)
  // foldRight(as, Nil: List[A])((a, filtered) => a match {
  //     case x if f(x) => Cons(x, filtered)
  //     case _ => filtered
  //   })


// myfilter in not tail recursive thus stack over flow  if you add @annotation.tailrec you will get error:
// could not optimize @tailrec annotated method myfilter: it contains a recursive call not in tail position
// it fails on (1 to 3000000).toList
// myfilter is not tail recursive thus stack owerflow on long list
// def myfilter[A](as: List[A])(f: A =>Boolean) : List[A] = as match{
// case Nil => Nil
// case (h::t) if(f(h)) => h::myfilter(t)(f)
// case (_::t) => myfilter(t)(f)
// }

  // Exercise 21

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = 
  //foldLeft(as, Nil: List[B])((flatMapped, a) => append(flatMapped, f(a)))
  foldRight(as, Nil: List[B])((xs, x)=> append(f(xs),x))
  //Appent takes two parameters if the first one is Nil it returns second one wheter it is List or List of List.
  //Therefore if we pass a List to flatMap the f function takes a List as input and if we pass a List of List the
  // f function takes List of List as expected. Now the important part is what function f returns. It returns a List[B]
  // which is also the final output that might be a list[Int] or List[List[Int]]. 
   

  // Exercise 22

  def filter1[A] (l: List[A]) (p: A => Boolean) :List[A] = 
  flatMap(l)(x => if (p(x)) List(x) else List()/*Nil*/)

  // Exercise 23

  def add (l: List[Int]) (r: List[Int]): List[Int] = (l, r) match{
    case (Cons(hl,tl), Cons(hr,tr)) => Cons(hl + hr, add(tl)(tr)) //hl + hr::add(tl)(tr)
    case _ => Nil // scala> val test = 4::5::6::Nil/List()    test: List[Int] = List(4, 5, 6)
  }


  // Exercise 24

  def zipWith[A,B,C] (f: (A,B)=>C) (l: List[A], r: List[B]): List[C] = (l, r) match{
    case (Cons(hl,tl), Cons(hr,tr)) => Cons(f(hl , hr), zipWith(f)(tl,tr)) //hl + hr::add(tl)(tr)
    case _ => Nil
  }



  def zip[A, B](as: List[A], bs: List[B]): List[(A, B)] = {
    @annotation.tailrec
    def loop(xs: List[A], ys: List[B], acc: List[(A, B)]): List[(A, B)] = {
      (xs, ys) match {
        case (Nil, _)                     => acc
        case (_, Nil)                     => acc
        case (Cons(h1, t1), Cons(h2, t2)) => loop(t1, t2, Cons((h1, h2), acc))
      }
    }
    loop(as, bs, List[(A, B)]())
  }

  // Exercise 25

  def hasSubsequence[A] (sup: List[A], sub: List[A]): Boolean = ??? 

 
  

  // Exercise 26

  def pascal (n: Int): List[Int] = ???

}
