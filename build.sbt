val scala3Version = "3.3.0"

lazy val Benchmark = config("bench") extend Test

lazy val root = project
  .in(file("."))
  .settings(
    name := "pudu",
    version := "0.2.1",

    scalaVersion := scala3Version,
    githubOwner := "jsnavar",
    githubRepository := "Pudu",
    scalacOptions ++= Seq("-Yretain-trees", "-deprecation"),
    coverageEnabled := true,

    libraryDependencies ++= Seq("org.scalameta" %% "munit" % "0.7.29" % Test,
                          ("com.storm-enroute" %% "scalameter" % "0.21" % Test).cross(CrossVersion.for3Use2_13),
                          ("com.storm-enroute" %% "scalameter-core" % "0.21" % Test).cross(CrossVersion.for3Use2_13),
                          "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.3"),

    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    Benchmark / logBuffered := false,
    Benchmark / parallelExecution := false,
    Benchmark / testOptions := Seq(Tests.Filter(s => s.contains("Bench"))),
    Benchmark / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Flat,
    Benchmark / fork := true,
    Benchmark / javaOptions ++= Seq("-Xmx8G", "-XX:+UseG1GC"),
    Test / testOptions += Tests.Filter(s => !s.contains("Bench")))
  .configs(Benchmark)
  .settings(inConfig(Benchmark)(Defaults.testTasks))
