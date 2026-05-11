lazy val doobieVersion = "0.8.0-RC1"

scalaVersion       := "2.13.18"
crossScalaVersions := Seq("2.13.18")

console / initialCommands := "import com.htmlism.lexorank._"

libraryDependencies += "org.tpolecat" %% "doobie-core" % doobieVersion
libraryDependencies += "org.tpolecat" %% "doobie-h2"   % doobieVersion

// scaladoc enhancements
Compile / doc / scalacOptions ++= Seq(
  "-groups",  // enable support for grouped members
  "-diagrams" // generate type hierarchy diagrams
)
