lazy val `fp-in-scala` = (project in file(".")).
  settings(
    organization := "net.paulgray",
    name := "fp-in-scala",
    version := "1.0.0",
    scalaVersion := "2.11.2"
  )

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

sourceDirectories in Compile += new File("source")
