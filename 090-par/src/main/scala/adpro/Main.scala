// Advanced Programming
// Andrzej Wąsowski, IT University of Copenhagen

package adpro
import java.util.concurrent.Executors

import Par._


//See "Main" as a Java-like "main" method. 
object Main extends App {
    println("Welcome to Parallel Computation, the ADPRO 050 class!!")

    // play with your functions below.
    val S = Executors.newFixedThreadPool(8)
    val ll = (1 to 5000).toList
    val rl = parFilter(ll)((t: Int) => true)
    println(rl(S))

//    Write a function that takes a list of paragraphs (a List[String] ) and returns
//      the total number of words across all paragraphs, in parallel. Generalize this
//    function as much as possible.

}
