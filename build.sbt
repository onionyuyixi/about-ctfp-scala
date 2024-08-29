ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "about-ctfp-scala",
    libraryDependencies ++= Seq(
      "org.scalaz" %% "scalaz-core" % "7.3.8",


    ),
    addCompilerPlugin(("org.typelevel" %% "kind-projector" % "0.13.3").cross(CrossVersion.full)),
    addCompilerPlugin(("com.olegpy" %% "better-monadic-for" % "0.3.1"))
  )
