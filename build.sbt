seq(Revolver.settings: _*) //for sbt-revolver
enablePlugins(ScalaJSPlugin)

lazy val root = project.in(file(".")).
  aggregate(lispjs, lispjvm).
  settings(
    publish := {},
    publishLocal := {},
    scalaVersion := "2.11.7"
  )

lazy val lisp = (crossProject in file(".")).
  settings(
    organization := "org.ankits",
    name := "lisp",
    version := "0.1.0",
    scalaVersion := "2.11.7",
    libraryDependencies ++= Seq(
      "com.github.pathikrit" %% "better-files" % "2.12.2",

      "com.lihaoyi" %%% "fastparse" % "0.3.4",
      "com.lihaoyi" %%% "pprint" % "0.3.6",

      "com.lihaoyi" % "ammonite-repl_2.11.7" % "0.5.2",
      "com.lihaoyi" %%% "utest" % "0.3.1" % "test"
    ),
    initialCommands in (Test, console) := """ammonite.repl.Main.run("")""",
    //initialCommands in console := autoImports,
    skip in packageJSDependencies := false,
    persistLauncher := true,

    testFrameworks += new TestFramework("utest.runner.Framework"),

    crossTarget in (Compile, fullOptJS) := file("js"),
    crossTarget in (Compile, fastOptJS) := file("js"),
    crossTarget in (Compile, packageJSDependencies) := file("js"),
    crossTarget in (Compile, packageScalaJSLauncher) := file("js"),
    crossTarget in (Compile, packageMinifiedJSDependencies) := file("js"),
    artifactPath in (Compile, fastOptJS) := ((crossTarget in (Compile, fastOptJS)).value /
      ((moduleName in fastOptJS).value + "-opt.js"))
  )
 .jsSettings(
    libraryDependencies ++= Seq(
      "com.github.japgolly.scalajs-react" %%% "core" % "0.10.0",
      "com.github.japgolly.scalajs-react" %%% "extra" % "0.10.0",
      "com.github.japgolly.scalacss" %%% "core" % "0.3.1",
      "com.github.japgolly.scalacss" %%% "ext-react" % "0.3.1"
    ),
    jsDependencies ++= Seq(
      "org.webjars.npm" % "react" % "0.14.1" / "react-with-addons.js" minified "react-with-addons.min.js" commonJSName "React",
      "org.webjars.npm" % "react-dom" % "0.14.1" / "react-dom.js" commonJSName "ReactDOM" minified "react-dom.min.js" dependsOn "react-with-addons.js",
      RuntimeDOM
    )
  )

lazy val lispjvm = lisp.jvm
lazy val lispjs = lisp.js

val autoImports: String =
  """
    |import better.files._
    |import java.io.{File => JFile, _}, java.nio._, java.nio.file._, java.nio.charset._
  """.trim.stripMargin

cancelable in Global := true
