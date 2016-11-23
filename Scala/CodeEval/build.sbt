name := "CodeEval"

version := "1.0"

scalaVersion := "2.11.8"

scalacOptions ++= Seq(
  "-target:jvm-1.8",
  "-encoding", "UTF-8",
  "-unchecked",            // Detailed type erasure warnings
  "-deprecation",          // Detailed deprecation warnings
  "-feature",              // Warnings about misused language features
  "-Xfuture",              // Turn on future language features - Disable adapted arguments and more - See: http://blog.threatstack.com/useful-scalac-options-for-better-scala-development-part-1
  "-Yno-adapted-args",     // Warns about adapted arguments
  "-Ywarn-dead-code",      // Dead code warnings
  "-Ywarn-numeric-widen",  // Warnings about implicit numeric widening (e.g. int to float)
  "-Ywarn-value-discard",  // Warnings about implicitly discarded values
  "-Ywarn-unused"          // Warnings about unused code (can have false positives)
)