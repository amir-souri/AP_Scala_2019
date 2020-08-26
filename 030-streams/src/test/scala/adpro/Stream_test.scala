package adpro
import org.scalatest.FunSuite
import Stream._

class Stream_test extends FunSuite {
  
  test("This one always works: (-1) * (-1) = 1") {
    assert((-1)*(-1)==1);
  }
  
  //Sanity check of exercise 1
  test("The first element of Stream.from(3) is 3") {
    assert(from(3).headOption().contains(3));
  }

  test("The second element of Stream.from(3) is 4") {
    assert(from(3).tail.headOption().contains(4));
  }

	test("The first element of Stream.to(3) is 3") {
    assert(to(3).headOption().contains(3));
  }
  
	test("The second element of Stream.to(3) is 2") {
    assert(to(3).tail.headOption().contains(2));
  }

	test("The first element of naturals is 0"){
		assert(naturals.headOption().contains(0))
	}


	test("The Stream(1,2,3).toList is List(1,2,3) "){
		  val l2 :Stream[Int]= cons(1, cons(2, cons (3, empty)))
			assert(l2.toList(0) == 1)
			assert(l2.toList(1) == 2)
			assert(l2.toList(2) == 3)


	}

	test("naturals.take(3)  is Stream(1,2,3) "){
		  val l2 :Stream[Int]= naturals.take(3) 
			assert(l2.toList(0) == 0)
			assert(l2.toList(1) == 1)
			assert(l2.toList(2) == 2)
  }

  test("naturals.drop(3) is Stream(3,4,5,...) "){
		  val l2 :Stream[Int]= naturals.drop(3) 
			assert(l2.headOption().contains(3))
      assert(l2.tail.headOption().contains(4))
      assert(l2.tail.tail.headOption().contains(5))

			//assert(l2.toList(2) == 5)
  }

  ////////////////////////////////////////////////////////////////////

  test("naturals.takeWhile(_ < 10) is Stream(0,1,2,3,4,5,6,7,8,9)"){
    val l2 :Stream[Int]= naturals.takeWhile(_ < 10)
    assert(l2.headOption().contains(0))
    assert (l2.toList(9) == 9)
    assert (l2.toList.length == 10)
  }
	
  test("forAll"){
    val l2 :Stream[Int] = Stream(1,2,3,4,0)
    val l3 :Stream[Int] = Stream(1,2,3,4,5)
    assert(l2.forAll(_ > 0) == false)
    assert(l3.forAll(_ > 0) == true)
    assert(naturals.forAll (_ < 0) == false) 
    //assert(naturals.forAll (_ >=0) == true)  // This will crash since natural number are infinity.
  }

 test("naturals.takeWhile2(_ < 10) is Stream(0,1,2,3,4,5,6,7,8,9)"){
    val l2 :Stream[Int]= naturals.takeWhile2(_ < 10)
    assert(l2.headOption().contains(0))
    assert (l2.toList(9) == 9)
    assert (l2.toList.length == 10)
  }

  test("headOption"){
    val l2 :Stream[Int] = Stream(1,2,3,4,0)
    assert(l2.headOption == l2.headOption2)
    assert(naturals.headOption == naturals.headOption2)
    assert(Empty.headOption2 == None)
  }

  test("map"){
    assert(naturals.map (_*2).drop (30).take (50).toList == (60 to 158 by 2).toList )
    //List(60, 62, 64, 66, 68, 70, 72, 74, 76, 78, 80, 82, 84, 86, 88, 90, 92, 94, 96, 98, 100,
    //102, 104, 106, 108, 110, 112, 114, 116, 118, 120, 122, 124, 126, 128, 130, 132, 134
    //136, 138, 140, 142, 144, 146, 148, 150, 152, 154, 156, 158)
  }

  test("filter") {
    assert(naturals.drop(42).filter (_%2 ==0).take (30).toList == (42 to 100 by 2).toList)
    //List(42, 44, 46, 48, 50, 52, 54, 56, 58, 60, 62, 64, 66, 68, 70,
    //72, 74, 76, 78, 80, 82, 84, 86, 88, 90, 92, 94, 96, 98, 100)
  }

  test("append"){
    naturals.append (naturals) //(useless, but should not crash)
    assert(naturals.take(10).append(naturals).take(20).toList == List(0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9))
  }

  test("flatMap"){
  //assert(naturals.flatMap (to _).take (100).toList == List(1) )
  //assert(naturals.flatMap(i => Stream(i,i)).take(100).toList ==  List("test"))
  //l2.flatMap(i => Stream(i,i)).toList
  assert(naturals.flatMap (x =>from (x)).take (100).toList == (0 to 99).toList )
  }

  test("unfold"){
    assert((unfold(10) { (x) => if (x == 0) None else Some(x, x - 1) }).toList == 
    Stream(10, 9, 8, 7, 6, 5, 4, 3, 2, 1).toList)
  }

  test("unfold another test"){
    assert((unfold(Stream(10,9,8,7,6,5)) { 
      (x) => if (x.headOption == Some(5)) None else Some(x.headOption().get, x.tail) }).toList == 
    Stream(10, 9, 8, 7, 6).toList)
  }


  test("fibs"){
    assert(fibs.take(100).toList ==fib2.take(100).toList)
  }

  test("fibs2"){
    assert(fibs.take(100).toList ==fib2.take(100).toList)
  }

  test("from2"){
    assert(from(1).take(1000000000).drop (41).take(10).toList ==
    from2(1).take(1000000000).drop (41).take(10).toList)
  }

  test("map2"){
    assert(naturals.map2 (_*2).drop (30).take (50).toList == (60 to 158 by 2).toList )
  }

  test("naturals.take2(3)  is Stream(1,2,3) "){
    val l2 :Stream[Int]= naturals.take2(3) 
    assert(l2.toList(0) == 0)
    assert(l2.toList(1) == 1)
    assert(l2.toList(2) == 2)
}

  test("naturals.takeWhile3(_ < 10) is Stream(0,1,2,3,4,5,6,7,8,9)"){
    val l2 :Stream[Int]= naturals.takeWhile3(_ < 10)
    assert(l2.headOption().contains(0))
    assert (l2.toList(9) == 9)
    assert (l2.toList.length == 10)
  }

  test("zipwith2"){
    assert(naturals.zipWith2[Int,Int] (_+_) (naturals).take(2000000000).take(20).toList == (0 to 38 by 2).toList )
  }
}