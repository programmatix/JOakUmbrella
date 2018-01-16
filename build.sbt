enablePlugins(ScalaJSPlugin)

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"
libraryDependencies += "com.lihaoyi" %%% "pprint" % "0.5.3"

import scala.sys.process._
val copy = taskKey[Unit]("")
val copyProd = taskKey[Unit]("")
copy := { "cp target\\scala-2.12\\ctranspiler-fastopt.js ..\\www\\hugo\\static\\js" ! }
copyProd := { "cp target\\scala-2.12\\ctranspiler-opt.js ..\\www\\hugo\\static\\js" ! }

//lazy val CParser = ProjectRef(file("CParser"), "CParser")
lazy val CParser = (project in file("CParser"))
lazy val CTranspiler = (project in file("."))
  .settings(
    name := "CTranspiler",
    scalaVersion := "2.12.4"
  ).dependsOn(CParser)

