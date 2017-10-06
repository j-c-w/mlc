import com.github.retronym.SbtOneJar._

name := "cmlc"
version := "0.0.1"
scalaVersion := "2.11.8"

libraryDependencies += "com.codecommit" %% "gll-combinators" % "2.3"
libraryDependencies += "org.rogach" %% "scallop" % "3.1.0"

mainClass in oneJar := Some("toplev.Toplev")

oneJarSettings
