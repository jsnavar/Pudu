val scala3Version = "3.2.2"

scalacOptions ++= Seq("-Yretain-trees")

lazy val root = project
  .in(file("."))
  .settings(
    name := "pudu",
    version := "0.2.1",

    scalaVersion := scala3Version,
    githubOwner := "jsnavar",
    githubRepository := "Pudu",
    jacocoReportSettings := JacocoReportSettings().withFormats(JacocoReportFormats.ScalaHTML)
                                                  .withThresholds(JacocoThresholds(line = 90)),
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )
