organization in ThisBuild := "com.github.fdietze"
name in ThisBuild := "vectory"
version in ThisBuild := "0.1.0-SNAPSHOT"

scalaVersion in ThisBuild := "2.12.1"
crossScalaVersions in ThisBuild := Seq("2.10.6", "2.11.8", "2.12.1")

lazy val root = project.in(file(".")).
  aggregate(vectoryJS, vectoryJVM).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val vectory = (crossProject.crossType(CrossType.Pure) in file("."))
  .settings(
    libraryDependencies ++= (
      "org.scala-js" %% "scalajs-stubs" % scalaJSVersion ::
      "org.specs2" %% "specs2-core" % "3.8.6" % "test" ::
      Nil
    ),

    scalacOptions in Test ++= Seq("-Yrangepos"), // for Specs2

    //TODO: wartremover

    initialCommands in console := """
    import vectory._
    """,

    scalacOptions ++= (
      "-encoding" :: "UTF-8" ::
      "-unchecked" ::
      "-deprecation" ::
      "-explaintypes" ::
      "-feature" ::
      "-language:_" ::
      Nil
    )
  )

lazy val vectoryJVM = vectory.jvm
lazy val vectoryJS = vectory.js
