// Advanced Programming
// Andrzej Wasowski, IT Univesity of Copenhagen
//
// 1. Introduction
//
// This is a superfast walk through the API designed in Chapter 6.
// This exercise assumes that you have read Chapter 6, and it attempts to get
// you up to speed with its main interface, so that we can work on chapter 8.

// We do not use this file in the exercises. It is provide for you as a
// refresher of the State and RNG stuff.  You can ignore it, if you do not need
// the refresher.
//
// Work through the following examples line by line, running them in the REPL,
// understanding and exercising the produced values.  Refer to Chapter 6 and to
// the State.scala file whenever you do not understand.

//Question
// 2. Build
//
// The simplest way is to run: 'sbt console' and then 'import adpro.Exercise1._`
//
// I build these files using sbt (see the toplevel README file for this week). If
// you want to build them directly, the instructions are below, but you will
// need to put all the files in the same directory to use the compiler directly
// (or otherwise figure out the right options).

// First compile State.scala
// $ fsc State.scala
//
// This file is designed so that you can compile it as a regular scala module
// (fsc Exercise1.scala).  This is useful for typechecking the entire file.
// However the intention is that you open the REPL and work through it
// expression by expression. So:
//
// $ scala -i
//
// and paste line by line into the repo (just skip the object declaration line).

package adpro

import fpinscala.state._


object Exercise1 { // this line not needed in REPL

  // RNG is the type of (R)andom (N)umber (G)enerators.  Its companion object
  // (also called RNG) has a factory called "Simple" that allows creating a
  // generator of pseudo random Int numbers.
  //Question// Why the type of rng1 is RNG not Simple?
  //Since you assigned a Simple type to a RNG type here. (Upcasting)
  // Upcasting is casting to a supertype, while downcasting is casting to a subtype.
  //val rng1 :RNG = RNG.Simple(42) // 42 is the seed
  //Basically you assigned a Simple type to a RNG type here. It is possible due to inheritance
  //:type rng1 
  //RNG

  val rng1 = RNG.Simple(42)//It is valid due to inheritance. Simple is subtype of RNG since Simple extends RNG (trait)
  //val rng1 = RNG.Simple(42)
  //rng1: RNG.Simple = Simple(42)

  //class Animals
  //class Dog extends Animals
  //def passDog(ad1 : Animals) =println("can pass since Dog is subtype (subclass) of Animals")

  // Whenever a RNG (trait) is needed, you can pass a Simple since Simple extends RNG (trait)


  // Get a pseudo random number of the generator first (this is the easiest way)
  val (x,rng2) = rng1.nextInt
  // Note that the expression is referentially transparent; (It returns the same value on each call with the 
  //certain input) try to call it several times. Incidentally this also explains why random number generators
  // are rather called "pseudo random number generators". Why?
  //Because you can keep track of its generated number


  // QUESTION 1: how do I get the next random number? (check in the REPL and make
  // sure that you indeed have got a different number)
  //
  // val ... = ...
  // val(x,rng3) = rng2.nextInt

  // The book uses this simple generator to implement a series of various
  // generators, including one for nonnegative integers (function
  // RNG.nonNegativeInt) and for doubles (function RNG.double). We will use them
  // below. Check briefly where they are in State.scala, and what are their
  // types. It is slightly less important how they work.
  // They are in the companion object. 
  //S => (A, S). They takes an RNG (state) and return 
  //The return value is a tuple containing both a pseudo-random integer (A) and the next `RNG` state.
  //
  // The book wraps a code generator in a state pattern.  The state pattern
  // needs a transition function of type RNG => (A, RNG).  So the random number
  // generator state will be the state of our automaton, and the automaton will
  // generate outputs of type A. The function defines how to move from one state
  // to another state producing an output.
  //
  //Question//
  //val s_random_int :State[Int,RNG] = State(_.nextInt)
  // value nextInt is not a member of Int
  //Is it becuse we defined State as case class State[S, +A](run: S => (A, S)) ?
  //answer: since (run: S => (A, S)) recall S is RNG which has nextInt method
  //when you do State[Int,RNG] = State(_.nextInt). _ becomes an Int 

  //Question//
  //Why _.nextInt ? why RNG.nextInt does not compile?
  //value nextInt is not a member of object fpinscala.state.RNG
//[error]   val s_random_int :State[RNG,Int] = State(RNG.nextInt)
//recall case class State[S, +A](run: S => (A, S)) so it needs a function that takes a RNG here and returns 
//(RNG, A)
//answer: Since RNG (companion object) does not have any nextInt.
//def nextInt: (Int, RNG). as you see nextInt does not take a parameter. but it is defined in the RNG trait

  //why _.nonNegativeInt does not compile? 
  //value nonNegativeInt is not a member of fpinscala.state.RNG
// [error]   val s_nonNegativeInt :State[RNG,Int]= State(_.nonNegativeInt)
//answer: Since trait RNG does not have any nonNegativeInt but RNG (companion object) has one.
//def nonNegativeInt(rng: RNG): (Int, RNG). as you see nonNegativeInt takes an RNG and returns (Int, RNG) which is
//properier as State parameter.
  
  // Now we can package our code generators as automata:
  val s_random_int :State[RNG,Int] = State(x => x.nextInt) // Instead of val(x,rng3) = rng2.nextInt
  val s_nonNegativeInt :State[RNG,Int]= State(RNG.nonNegativeInt) //RNG.nonNegativeInt(rng2)
  val s_double :State[RNG,Double] = State(RNG.double) // RNG.double(rng2) (s => (A,S)) (RNG => (Double,RNG))

  // State gives as a uniform interface to all these generators; The interface
  // is generic, so regardless of what is being randomly generated we access it
  // in the same way.  Give a RNG to a state object and it will produce the next
  // RNG (or more intutiively it will produce the next state of this RNG). Try
  // using them as below:                                              ^

  //Is run a method or constructor?
  s_random_int.run (rng2) 
  s_nonNegativeInt.run (rng2)// s_nonNegativeInt.run is of type State[RNG,Int] then it has a run constructor which is
  // a function S => (A,S). We allready pass RNG.nonNegativeInt to s_nonNegativeInt (look at a few line above)
  //There for it is waiting for a RNG. Once you feed a RNG to run, it returns a tuple of (Int, RNG) i.e it is
  //S => (A,S) that here S/state is RNG and A is Int (nonNegativeInt) .
  s_double.run (rng2)

  // QUESTION 2: once you are comfortable with the above, take one of the other
  // generator functions from State.scala, wrap them into a state, and use the
  // above interface to get a random value out of them.
  //
  // val s_random_x : ... = State (...)
  // val random_x : ... = ....
  val s_random_boolean : State[RNG, Boolean] = State(RNG.boolean) 
  s_random_boolean.run(rng2)
  //NB A can be everything! Here A is a tuple of (Double, Double, Double)
  val s_random_double3 : State[RNG, (Double, Double, Double)] = State(RNG.double3)
  s_random_double3.run(rng2)

  //Question What do you mean! Could you please give me an example?
  // This wrapping makes as independent of the names of the functions of the
  // actual generators, so we can write generic functions for all generators,
  // or we can use even more generic functions implemented just in terms of
  // states.

  // Note that there seems to be a relation between State and Stream.  A
  // continuous execution of State generates a series (a trace) that can be
  // represented using a Stream.  We will use this observation below, to build
  // Streams of random numbers.

  // The following is an example linking States and Streams from the previous
  // exercise session. We will use the above state interface to wrap all random
  // generators into streams.

  // Our stream will be "delayed". It will await a seed (or rather an
  // initialized RNG) to start generation.  Note: I am using standard library
  // streams (which behave as expected)
  def state2stream[A] (s :State[RNG,A]) (seed :RNG) :Stream[A] =
    s.run(seed) match { case (n,s1) => n #:: state2stream (s) (s1) }
    //def #::[B >: A](elem: B): Stream[B]
    //Construct a Stream consisting of a given first element followed by elements from another Stream. 

    //Let use it
    val streamOfNonNegativeInt = state2stream(State(RNG.nonNegativeInt))(rng2)
    streamOfNonNegativeInt.take(7).toList

  // the function basically takes a transition function and converts it to a
  // stream (it could be written polumorphically  to work for any state.

  // QUESTION 3: generalize the above function to work for any State object, not
  // just for state pattern applied to the random number generators.

  def GenericState2stream[A,S] (s :State[S,A]) (seed :S) :Stream[A] =
    s.run(seed) match { case (n,s1) => Stream.cons( n, GenericState2stream (s) (s1) ) }
    //Let use it
    val streamOfGenericState2stream = GenericState2stream(State(RNG.nonNegativeInt))(rng2)
    streamOfGenericState2stream.take(7).toList

    //It works fine as spected
//     scala>     streamOfGenericState2stream.take(7).toList
// res2: List[Int] = List(1281479696, 340305901, 2015756019, 1770001318, 1934589058, 1015914512, 1163632440)

// scala>     streamOfGenericState2stream.take(7).toList
// res3: List[Int] = List(1281479696, 340305901, 2015756019, 1770001318, 1934589058, 1015914512, 1163632440)

// scala>     streamOfGenericState2stream.take(7).toList
// res4: List[Int] = List(1281479696, 340305901, 2015756019, 1770001318, 1934589058, 1015914512, 1163632440)



  // This hack allows us to hide the state of the random number generator in the
  // stream. We just work with a stream of random numbers declaratively from now
  // on, forgetting that there is some state of the random number generator
  // passed around.

  val random_integers = state2stream[Int] (s_random_int) (RNG.Simple(42))
  // now this is an easy way to generate 10 random numbers
  random_integers.take(10).toList

  // The same code that works with random integer stream can work with no change
  // with random stream of non-negative integers (the stream is computed using
  // the same state2stream function)
  val random_non_negative_integers =
    state2stream[Int] (s_nonNegativeInt) (RNG.Simple(42))

  random_non_negative_integers.take(10).toList

  // And we can use the same function to create a stream of random Doubles
  val random_doubles = state2stream[Double] (s_double) (RNG.Simple(42))
  random_doubles.take(10).toList

  // Or even Stream of tuples of random Doubles
  val random_double3 = state2stream[(Double, Double, Double)] (s_random_double3) (RNG.Simple(42))
  random_double3.take(10).toList

  // If you need a reasonably reliable seed, so that you do not get exactly
  // stream each time, use currentTimeMillis:
  val random_integers_no_RT =
    state2stream[Int] (s_random_int) (RNG.Simple(System.currentTimeMillis.toInt))

  // The above expression is of course not referentially transparent. ((((If we
  // substitute the computation for the identifier in each reference place, we
  // will get a different value each time!)))) The one below already is RT; it will
  // retain the same stream value throughout the program)
  random_integers_no_RT.take(10).toList

  // QUESTION 4: Now repeat the above by taking the random generator function
  // that you used in Question 2, combine it with the above API to create a
  // stream of random values that it generates. Access the stream to compute
  // several random values.
  val random_integers_no_RT_boolean =
    state2stream[Boolean] (s_random_boolean) (RNG.Simple(System.currentTimeMillis.toInt))

    random_integers_no_RT_boolean.take(12).toList
}

// vim:cc=80:foldmethod=indent:foldenable:tw=80
