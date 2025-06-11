ThisBuild / scalaVersion := "3.5.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % Test
Compile / unmanagedSourceDirectories := Seq(baseDirectory.value / "ready-for-use")
Test / unmanagedSourceDirectories := Seq(baseDirectory.value / "tests")
