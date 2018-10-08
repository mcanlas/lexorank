lazy val doobieVersion = "0.5.3"

scalafmtOnCompile := true

scalaVersion := "2.12.7"
crossScalaVersions := Seq("2.11.12", "2.12.7")

initialCommands in console := "import com.htmlism.lexorank._"

// for cats
scalacOptions += "-Ypartial-unification"

// for compatibility, allow doobie to dictate what version of cats and cats effect to use
libraryDependencies += "org.typelevel" %% "mouse" % "0.18"

libraryDependencies += "org.tpolecat" %% "doobie-core" % doobieVersion
libraryDependencies += "org.tpolecat" %% "doobie-h2"   % doobieVersion

libraryDependencies += "org.scalatest"  %% "scalatest"        % "3.0.5"       % "test"
libraryDependencies += "org.tpolecat"   %% "doobie-scalatest" % doobieVersion % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck"       % "1.14.0"      % "test"
