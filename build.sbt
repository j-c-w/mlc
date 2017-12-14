import com.github.retronym.SbtOneJar._

name := "cmlc"
version := "0.0.1"
scalaVersion := "2.11.8"

libraryDependencies += "org.rogach" %% "scallop" % "3.1.0"
// For pretty printing case classes
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"
// For utility methods
libraryDependencies += "commons-lang" % "commons-lang" % "2.6"

mainClass in oneJar := Some("toplev.Toplev")

oneJarSettings
