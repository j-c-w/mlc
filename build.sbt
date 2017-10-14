import com.github.retronym.SbtOneJar._

name := "cmlc"
version := "0.0.1"
scalaVersion := "2.11.8"

libraryDependencies += "org.rogach" %% "scallop" % "3.1.0"
// For pretty printing case classes
libraryDependencies += "com.github.nikita-volkov" % "sext" % "0.2.4"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"

mainClass in oneJar := Some("toplev.Toplev")

oneJarSettings
