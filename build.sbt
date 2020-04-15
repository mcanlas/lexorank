lazy val doobieVersion = "0.8.0-RC1"

scalafmtOnCompile := true

scalaVersion := "2.13.1"
crossScalaVersions := Seq("2.12.10", "2.13.1")

initialCommands in console := "import com.htmlism.lexorank._"

// for compatibility, allow doobie to dictate what version of cats and cats effect to use
libraryDependencies += "org.typelevel" %% "mouse" % "0.24"

libraryDependencies += "org.tpolecat" %% "doobie-core" % doobieVersion
libraryDependencies += "org.tpolecat" %% "doobie-h2"   % doobieVersion

libraryDependencies += "org.scalatest"  %% "scalatest"        % "3.1.1"       % "test"
libraryDependencies += "org.tpolecat"   %% "doobie-scalatest" % doobieVersion % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck"       % "1.14.2"      % "test"
libraryDependencies += "org.scalatestplus" %% "scalacheck-1-14" % "3.1.1.2"   % "test"

// scaladoc enhancements
scalacOptions in (Compile, doc) ++= Seq(
  "-groups",  // enable support for grouped members
  "-diagrams" // generate type hierarchy diagrams
)
