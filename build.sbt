lazy val vectory = (crossProject.crossType(CrossType.Pure))
  .settings(
    organization := "com.github.fdietze",
    name := "vectory",
    version := "master-SNAPSHOT",
    crossScalaVersions := Seq("2.10.7", "2.11.12", "2.12.8"),
    scalaVersion := crossScalaVersions.value.last,


    resolvers += ("jitpack" at "https://jitpack.io"),
    libraryDependencies ++= (
      "org.scalatest" %%% "scalatest" % "3.0.5" % Test ::
      "com.github.fdietze.flatland" %%% "flatland" % "52e497d" ::
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
    "-Xcheckinit" ::
    "-Xfuture" ::
    "-Xlint:-unused" ::
    "-Ypartial-unification" ::
    "-Yno-adapted-args" ::
    "-Ywarn-infer-any" ::
    "-Ywarn-value-discard" ::
    "-Ywarn-nullary-override" ::
    "-Ywarn-nullary-unit" ::
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
