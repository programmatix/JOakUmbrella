//enablePlugins(ScalaJSPlugin)
enablePlugins(JavaAppPackaging)

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"
//libraryDependencies += "com.lihaoyi" %%% "pprint" % "0.5.3"
//libraryDependencies += "com.lihaoyi" %%% "fastparse" % "1.0.0"
libraryDependencies += "com.lihaoyi" %% "pprint" % "0.5.3"
libraryDependencies += "com.lihaoyi" %% "fastparse" % "1.0.0"


//mainClass in Compile := Some("jvm.JVM")

exportJars := true


//lazy val CParser = ProjectRef(file("CParser"), "CParser")
//lazy val CParser = ProjectRef(file("CParser"), "cparser")
//lazy val CParserWebDemo = ProjectRef(file("CParserWebDemo"), "CParserWebDemo")
//
//lazy val CParserJS = LocalProject("CParserJS")
//lazy val CParserJVM = LocalProject("CParserJVM")

//lazy val CTranspiler = (project in file("."))
//  .settings(
//    name := "CTranspiler",
//    scalaVersion := "2.12.4",
//    test in assembly := {}
//  ).dependsOn(CParser)

lazy val CTranspiler = (project in file("."))
  .settings(
    name := "CTranspiler",
    scalaVersion := "2.12.4",
    test in assembly := {}
  )

//val CTranspilerJS = CTranspiler.js.dependsOn(CParser)
//val CTranspilerJVM = CTranspiler.jvm.settings(
////  (resources in Compile) += {
////    (fastOptJS in (exampleJS, Compile)).value
////    (artifactPath in (exampleJS, Compile, fastOptJS)).value
////  }
//).dependsOn(CParser)