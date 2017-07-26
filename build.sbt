name := "hackerrank"

version := "1.0"

scalaVersion := "2.12.2"

libraryDependencies ++= (
  "org.scalatest" %% "scalatest" % "3.0.3" ::
    "com.storm-enroute" %% "scalameter-core" % "0.8.2" ::
    Nil
  )

//testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

parallelExecution in Test := false