name := "hackerrank"

version := "1.0"

scalaVersion := "2.12.3"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.3"
libraryDependencies += "com.storm-enroute" %% "scalameter-core" % "0.8.2"

//testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

parallelExecution in Test := false