enablePlugins(ScalaJSPlugin)
name := "CCompiler"

version := "0.1"

scalaVersion := "2.12.4"

// Make IntelliJ and SBT build to different dirs
//sbtide.Keys.ideOutputDirectory := Some(new File(baseDirectory.value + "/intellij-out"))
//sbtide.Keys.ideOutputDirectory in Test := Some(new File(baseDirectory.value + "/intellij-out-test"))
//sbtide.Keys.ideExcludedDirectories := Seq(new File(baseDirectory.value + "/target"))

// This is an application with a main method
//scalaJSUseMainModuleInitializer := true

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"
libraryDependencies += "com.lihaoyi" %%% "fastparse" % "1.0.0"
