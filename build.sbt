val scala3Version = "3.2.2"

scalacOptions ++= Seq("-Yretain-trees")

lazy val root = project
  .in(file("."))
  .settings(
    name := "pudu",
    version := "0.1",

    scalaVersion := scala3Version,
    githubOwner := "jsnavar",
    githubRepository := "Pudu",

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )
