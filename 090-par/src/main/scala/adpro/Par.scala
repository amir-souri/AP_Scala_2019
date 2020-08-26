package adpro
import java.util.concurrent._

import scala.language.implicitConversions

//NB. This implementation of Par, does not work with fixed-size thread pools. Indeed, It is prone to deadlock.
//The textbook made use of Actor/message to overcome this issue in section 7.4.4. (p 115-119)

// Work through the file top-down, following the exercises from the week's
// sheet.  Uncomment and complete code fragments.

object Par {

  type Par[A] = ExecutorService => Future[A]
  def run[A] (s: ExecutorService) (a: Par[A]) : Future[A] = a(s)

  case class UnitFuture[A] (get: A) extends Future[A] {
    def isDone = true
    def get (timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel (evenIfRunning: Boolean) : Boolean = false
  }

  def unit[A] (a: A) :Par[A] = (es: ExecutorService) => UnitFuture(a)

  def map2[A,B,C] (a: Par[A], b: Par[B]) (f: (A,B) => C) : Par[C] =
    (es: ExecutorService) => {
      val af = a (es)
      val bf = b (es)
      UnitFuture (f(af.get, bf.get))
    }

  def fork[A] (a: => Par[A]) : Par[A] = es => es.submit(
    new Callable[A] { def call = a(es).get }
  )

  def lazyUnit[A] (a: =>A) : Par[A] = fork(unit(a))

  // Exercise 1 (CB7.4)
  //asyncF converts any function A => B to one that evaluates
  //its result asynchronously (so it spawns a separate thread).
  def asyncF[A,B] (f: A => B) : A => Par[B] = (a: A) => lazyUnit(f(a))

  // map is shown in the book

  def map[A,B] (pa: Par[A]) (f: A => B) : Par[B] =
    map2 (pa,unit (())) ((a,_) => f(a))

  // Exercise 2 (CB7.5)

   def sequence[A] (ps: List[Par[A]]): Par[List[A]] =
     ps.foldRight(lazyUnit(List[A]())) ( map2(_,_) (_::_))

  // Exercise 3 (CB7.6)

  // this is shown in the book:

   def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
     val fbs: List[Par[B]] = ps.map(asyncF(f))
     sequence(fbs)
   }

   def parFilter[A](as: List[A])(f: A => Boolean) : Par[List[A]] = {
     val pars : List[Par[List[A]]] = as.map(asyncF((a: A) => if (f(a)) List(a) else List()))
     map(sequence(pars))(_.flatten)
   }

  // Exercise 4: implement map3 using map2

   def map3[A,B,C,D] (pa :Par[A], pb: Par[B], pc: Par[C]) (f: (A,B,C) => D) :Par[D]  = {
     def partialCurry(a: A, b: B)(c: C): D = f(a, b, c)
     val pc2d: Par[C => D] = map2(pa, pb)(partialCurry) //(a, b) => partialCurry(a, b)
     def applyFunc(func: C => D, c: C): D = func(c)
     map2(pc2d, pc)(applyFunc)  //(c2d, c) => applyFunc(c2d, c)
   }

  // shown in the book

   def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = p(e).get == p2(e).get

  // Exercise 5 (CB7.11)

   def choiceN[A] (n: Par[Int]) (choices: List[Par[A]]) :Par[A] = es => {
     val index = run(es)(n).get
     choices(index)    (es) //run(es)(choices(index)) or even val choice: Par[A] = choices(index) and then   choice(es)
   }
   def choice[A] (cond: Par[Boolean]) (t: Par[A], f: Par[A]) : Par[A] =
     choiceN(map(cond)(if (_) 0 else 1))   (List(t,f)) // choiceN(Par.map(cond)(boo => if (boo) 0 else 1))

//  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
//    es =>
//      if (run(es)(cond).get) t(es)
//      else f(es)

  // Exercise 6 (CB7.13)
  /*Note: In search for an "aha" moment, compare the type of the chooser, with the types of Option.flatMap ,
  Stream.flatMap , List.flatMap and State.flatMap . Observe that the chooser is used to com-
    pose (sequence) to parallel computations here.*/

   def chooser[A,B] (pa: Par[A]) (choices: A => Par[B]): Par[B] = es => {
     val n = run(es)(pa).get  // val k = Par.run(es)(pa).get
     choices(n)   (es) //run(es)(choices(k)) or even val choice = choices(n) and then  choice(es)
     //choices(run(es)(pa).get)(es)
   }

   //`chooser` is usually called `flatMap` or `bind`. Read page 122 in the textbook.
   def flatMap[A,B](p: Par[A])(choices: A => Par[B]): Par[B] = es => {
      val k = run(es)(p).get
      run(es)(choices(k))
    }


  def choiceNviaChooser[A] (n: Par[Int]) (choices: List[Par[A]]) :Par[A] =
     chooser(n) (choices) //chooser(n)(choices(_))
    //flatMap(n)(i => choices(i))

   def choiceViaChooser[A] (cond: Par[Boolean]) (t: Par[A], f: Par[A]) : Par[A] =
     chooser(cond) (bool => if (bool) t else f)
    //flatMap(cond)(boo => if (boo) t else f)

  // Exercise 7 (CB7.14)
  /*Compare the type of join with the type of List.flatten (and the relation of join to chooser
    against the relation of List.flatten to List.flatMap ).*/

   def join[A] (a : Par[Par[A]]) :Par[A] =
     es => run(es)(run(es)(a).get())  // es => run(es)(a).get()(es)


//  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
//    flatMap(a)((x: Par[A]) => x)
//
//  def flatMapViaJoin[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
//    join(map(p)(f))

  class ParOps[A](p: Par[A]) {

  }

  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)
}