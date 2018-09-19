lazy val doobieVersion = "0.5.3"

scalaVersion := "2.12.6"
crossScalaVersions := Seq("2.11.12", "2.12.6")

initialCommands in console := "import com.htmlism.lexorank._"

libraryDependencies += "org.typelevel" %% "cats-core"   % "1.4.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "1.0.0"
libraryDependencies += "org.typelevel" %% "mouse"       % "0.18"

libraryDependencies += "org.tpolecat" %% "doobie-core" % doobieVersion
libraryDependencies += "org.tpolecat" %% "doobie-h2"   % doobieVersion

libraryDependencies += "org.scalatest"  %% "scalatest"          % "3.0.5"       % "test"
libraryDependencies += "org.tpolecat"   %% "doobie-scalatest"   % doobieVersion % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck"         % "1.14.0"      % "test"
