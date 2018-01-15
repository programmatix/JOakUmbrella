enablePlugins(ScalaJSPlugin)

// Make IntelliJ and SBT build to different dirs
//sbtide.Keys.ideOutputDirectory := Some(new File(baseDirectory.value + "/intellij-out"))
//sbtide.Keys.ideOutputDirectory in Test := Some(new File(baseDirectory.value + "/intellij-out-test"))
//sbtide.Keys.ideExcludedDirectories := Seq(new File(baseDirectory.value + "/target"))

// This is an application with a main method
//scalaJSUseMainModuleInitializer := true

//libraryDependencies += "org.scala-lang.modules" %% "scala-CParser-combinators" % "1.0.6"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"
//libraryDependencies += "com.lihaoyi" %%% "fastparse" % "1.0.0"

//lazy val CParser = ProjectRef(file("CParser"), "CParser")
lazy val CParser = (project in file("CParser"))
lazy val CTranspiler = (project in file("."))
  .settings(
    name := "CTranspiler",
    scalaVersion := "2.12.4"
  ).dependsOn(CParser)

