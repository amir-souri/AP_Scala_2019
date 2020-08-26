// Advanced Programming 2015
// Andrzej Wasowski, IT University of Copenhagen
package fpinscala.monads
import scala.language.higherKinds

trait Functor[F[_]] {

  def map[A,B] (fa: F[A]) (f: A => B) :F[B]

  def distribute[A,B] (fab: F[(A,B)]): (F[A],F[B]) =
    (map (fab) (_._1), map (fab)(_._2))

  def codistribute[A,B] (e :Either[F[A],F[B]]): F[Either[A,B]] = e match {
    case Left(fa) => map (fa) (Left(_))
    case Right(fb) => map (fb) (Right(_))
  }

}

object Functor {

  val ListFunctor = new Functor[List] {
    def map[A,B] (as: List[A]) (f: A => B): List[B] = as.map (f)
  }

  // Exercise 10

   val OptionFunctor = new Functor[Option] {
   def map[A,B] (oa: Option[A]) (f: A => B) : Option[B] = oa map f
   }

}

trait Monad[F[_]] {

  def unit[A]  (a: => A): F[A]
  def flatMap[A,B] (ma: F[A]) (f: A => F[B]) :F[B]

  def map[A,B] (ma: F[A]) (f: A => B) :F[B] =
    flatMap (ma) (a => unit (f(a)))

  def map2[A, B, C] (ma: F[A], mb: F[B]) (f: (A,B) => C): F[C] =
    flatMap (ma) (a => map (mb) (b => f(a,b)))

  // Exercise 13 (CB11.3)


  ///def sequence[A] (aos: List[Option[A]]): Option[List[A]] = //Exercise (4.4)
  ///aos.foldRight[Option[List[A]]] (Some(Nil))((oa, ob) => map2(oa, ob)(_ :: _))
  ///{
    ///if (aos.length == 0)
      ///Some(Nil)
   ///else
      ///aos.init.foldRight(aos.last.map(a => List(a)))((a, b) => b.flatMap(bn => a.flatMap(an => Some(an :: bn))))
  ///}

   def sequence[A] (lfa: List[F[A]]): F[List[A]] =
     lfa.foldRight(unit(List[A]()))((fa, fla) => map2(fa, fla)(_ :: _))
//   lfa.foldRight(unit(List[A]()))((fa: F[A], z: F[List[A]]) => map2(fa, z) (_ :: _))

  // traverse seems to simply sequence results of mapping.  I do not think that
  // it appeared in our part. You can uncomment it once you have sequence.

  ///def traverse[A,B] (as: List[A]) (f :A => Option[B]): Option[List[B]] = // Exercise (4.5)
    ///as.foldRight[Option[List[B]]](Some(Nil))((x, y) => map2(f(x), y)(_ :: _))


   def traverse[A,B] (la: List[A]) (f: A => F[B]): F[List[B]] =
     sequence(la.map (f))
//     la.foldRight(unit(List[B]()))((a: A, flb: F[List[B]]) => map2(f(a), flb)(_ :: _))

  // Exercise 14 (CB11.4)

   def replicateM[A] (n: Int, ma: F[A]): F[List[A]] = //flatMap(ma)(a => sequence(List.fill(n)(ma)))
     (sequence(List.fill(n)(ma)))

  /// Recursive version:
  ///def _replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    ///if (n <= 0) unit(List[A]()) else map2(ma, _replicateM(n - 1, ma))(_ :: _)

  def join[A] (mma: F[F[A]]): F[A] = flatMap (mma) (ma => ma)  //this.flatMap (mma) (ma => ma)

  // Exercise 15 is solved in MonadSpec.scala

  // Exercise 16 (CB11.7)

   def compose[A,B,C] (f: A => F[B], g: B => F[C]): A => F[C] =
//      a => flatMap(f(a))(b => g(b))
     a => flatMap(f(a))(g)

}

object Monad {

  // Exercise 12 (CB11.1)
  ///You have already defined `unit` and `flatMap` for these types. The solution is to simply call them
  ///from your `Monad` implementation.


   val optionMonad = new Monad[Option] {
     def unit[A](a: => A) = Some(a)
     def flatMap[A,B] (oa: Option[A]) (f: A => Option[B]) : Option[B] =
       oa.flatMap(f)
   }

   val listMonad = new Monad[List] {
     def unit[A](a: => A) = List(a)
     def flatMap[A,B] (la: List[A]) (f: A => List[B]) : List[B] =
       la.flatMap(f)
   }

///  val parMonad = new Monad[Par] {
///    def unit[A](a: => A) = Par.unit(a)
///    def flatMap[A,B](ma: Par[A])(f: A => Par[B]) = Par.flatMap(ma)(f)
///  }


///  val streamMonad = new Monad[Stream] {
///    def unit[A](a: => A) = Stream(a)
///    def flatMap[A,B](ma: Stream[A])(f: A => Stream[B]) = ma flatMap f
///  }


///  def parserMonad[P[+_]](p: Parsers[P]) = new Monad[P] {
///    def unit[A](a: => A) = p.succeed(a)
///    def flatMap[A,B](ma: P[A])(f: A => P[B]) = p.flatMap(ma)(f)
///  }

}
