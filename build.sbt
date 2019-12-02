lazy val vectory = (crossProject.crossType(CrossType.Pure))
  .settings(
    organization := "com.github.fdietze",
    name := "vectory",
    version := "master-SNAPSHOT",
    crossScalaVersions := Seq("2.11.12", "2.12.8", "2.13.0"),
    scalaVersion := crossScalaVersions.value.last,


    resolvers += ("jitpack" at "https://jitpack.io"),
    libraryDependencies ++= (
      "org.scalatest" %%% "scalatest" % "3.1.0" % Test ::
      "com.github.fdietze.flatland" %%% "flatland" % "7868da7" ::
      Nil
    ),

  scalaJSStage in Test := FullOptStage,

  initialCommands in console := """
  import vectory._
  """,

  scalacOptions ++=
    "-encoding" :: "UTF-8" ::
    "-unchecked" ::
    "-deprecation" ::
    "-explaintypes" ::
    "-feature" ::
    "-language:_" ::
    /* "-Xcheckinit" :: */
    "-Ywarn-value-discard" ::
    Nil,
  )
  .jvmSettings(
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % scalaJSVersion % "provided"
  )
  .jsSettings(
    scalacOptions += {
      val local = baseDirectory.value.toURI
      val remote = s"https://raw.githubusercontent.com/fdietze/vectory/${git.gitHeadCommit.value.get}/"
      s"-P:scalajs:mapSourceURI:$local->$remote"
    }
  )

lazy val jvm = vectory.jvm
lazy val js = vectory.js

Global / onChangedBuildSource := ReloadOnSourceChanges
