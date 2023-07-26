lazy val doobieVersion = "0.8.0-RC1"

scalaVersion       := "2.13.11"
crossScalaVersions := Seq("2.12.18", "2.13.11")

console / initialCommands := "import com.htmlism.lexorank._"

libraryDependencies += "org.tpolecat" %% "doobie-core" % doobieVersion
libraryDependencies += "org.tpolecat" %% "doobie-h2"   % doobieVersion

libraryDependencies += "org.scalatest"     %% "scalatest"        % "3.2.16"      % "test"
libraryDependencies += "org.tpolecat"      %% "doobie-scalatest" % doobieVersion % "test"
libraryDependencies += "org.scalacheck"    %% "scalacheck"       % "1.16.0"      % "test"
libraryDependencies += "org.scalatestplus" %% "scalacheck-1-14"  % "3.1.1.1"     % "test"

// scaladoc enhancements
Compile / doc / scalacOptions ++= Seq(
  "-groups",  // enable support for grouped members
  "-diagrams" // generate type hierarchy diagrams
)
