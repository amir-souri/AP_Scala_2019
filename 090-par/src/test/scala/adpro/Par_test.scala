package adpro
import java.util.concurrent.Executors

import org.scalatest.FunSuite
import Par._
import adpro.Main.{S, rl}


class Stream_test extends FunSuite {
  
  test("This one always works: (-1) * (-1) = 1") {
    assert((-1)*(-1)==1);
  }

  //write  your tests below.

  test( "The output of parFilter with f that returns allways true, is the sane as the input ") {
    val S = Executors.newFixedThreadPool(8)
    val ll = (1 to 50).toList
    val rl = parFilter(ll)((t: Int) => true)
    assert(rl(S) == UnitFuture(ll))
  }


  test( "The output of parFilter with f that returns allways false, is an empty list ") {
    val S = Executors.newFixedThreadPool(8)
    val ll = (1 to 50).toList
    val rl = parFilter(ll)((t: Int) => false)
    assert(rl(S) == UnitFuture(List()))
  }

	




}