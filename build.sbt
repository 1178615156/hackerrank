name := "hackerrank"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= (
  "org.scalatest" % "scalatest_2.11" % "2.2.4" ::
    "net.databinder.dispatch" %% "dispatch-core" % "0.11.2" ::
    "com.storm-enroute" %% "scalameter-core" % "0.7" ::
    "com.storm-enroute" %% "scalameter" % "0.7" ::
    Nil

  ).map(_.withSources())

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

parallelExecution in Test := false