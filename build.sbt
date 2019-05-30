name := "egt"

version := "0.1"

scalaVersion := "2.12.8"

mainClass in (Compile, packageBin) := Some("intersectionaldisadvantage.Main")

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
