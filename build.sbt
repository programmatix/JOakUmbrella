lazy val JOakAnalyser = ProjectRef(file("JOakAnalyser"), "JOakAnalyser")
lazy val JOakClassFiles = ProjectRef(file("JOakClassFiles"), "JOakClassFiles")
lazy val JOakJVM = ProjectRef(file("JOakJVM"), "JOakJVM")

lazy val JOakUmbrella = (project in file("."))
  .settings(
    name := "JOakUmbrella",
    scalaVersion := "2.12.4"
  ).dependsOn(JOakAnalyser, JOakClassFiles, JOakJVM)

