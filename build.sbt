name := "hackerrank"

version := "1.0"

scalaVersion := "2.11.7"


libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4"

libraryDependencies += "net.databinder.dispatch" %% "dispatch-core" % "0.11.2"

libraryDependencies += "com.storm-enroute" %% "scalameter-core" % "0.7"

libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.7"

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

parallelExecution in Test := false