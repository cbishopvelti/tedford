lazy val root = (project in file(".")).
  settings(
    name := "capgemini",
    version := "1.0",
    scalaVersion := "2.12.1"
  )

libraryDependencies += "org.specs2" %% "specs2-core" % "3.8.6" % "test"

scalacOptions in Test ++= Seq("-Yrangepos")
