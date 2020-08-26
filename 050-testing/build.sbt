name := "testing"

scalaVersion := "2.12.9"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

// Buffered Output:
// By default, sbt buffers log output for each suite until all tests for that suite complete and causing "spurty" 
// output. We recommend you disable sbt's log buffering so you can enjoy ScalaTest's built-in event buffering 
// algorithm, which shows the events of one suite as they occur until that suite either completes or a timeout 
// occurs, at which point ScalaTest switches a different suite's events. Just add this to your sbt build: 
logBuffered in Test := false

// Running Suites in Parallel:

// With the proliferation of multi-core architectures, and the often parallelizable nature of tests, it is common
//  to run tests in parallel. Sbt by default runs suites in parallel, using its own thread pool. In case you need
//   to run your suites serially, you can add the following to your sbt build file:

// parallelExecution in Test := false

// ScalaTest uses its own thread pool to run ParallelTestExecution suites, its pool size is determined by the 
// following formula:

// Pool Size = Available Processors x 2