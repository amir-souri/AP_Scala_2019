// Advanced Programming
// Andrzej Wąsowski, IT University of Copenhagen

package adpro
import Stream._


//See "Main" as a Java-like "main" method. 
object Main extends App {
    
    println("Welcome to Streams, the ADPRO 030 class!!")

    val l1 :Stream[Int] = empty

    val l2 :Stream[Int]= cons(1, cons(2, cons (3, empty)))

    println (l1.headOption)
    println (l2.headOption)
    println(l2)
    

    //println(naturals.flatMap (to _).take (100).toList)
    //println(fibs.take(100).toList)
    
        //println(l2.flatMap(i => Stream(i,i)).toList)

    //println( from2(1).take(20)toList )

    //def zipWith2[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
     println(naturals.zipWith2[Int,Int] (_+_) (naturals).take(2000000000).take(20).toList)

    //////////////////////////////////////////

    //val takeWhile = 
}
