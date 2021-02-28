Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / organization := "codes.quine.labo"

ThisBuild / scalaVersion := "2.13.4"
ThisBuild / scalacOptions ++= Seq(
  "-encoding",
  "UTF-8",
  "-feature",
  "-deprecation",
  "-Wunused"
)

// Scalafix config:
ThisBuild / scalafixScalaBinaryVersion := "2.13"
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision
ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.5.0"
ThisBuild / scalafixDependencies += "com.github.vovapolu" %% "scaluzzi" % "0.1.17"

lazy val root = project
  .in(file("."))
  .aggregate(core, cats)

lazy val core = project
  .in(file("modules/uchu-core"))
  .settings(
    name := "uchu-core",
    console / initialCommands := """
      |import codes.quine.labo.uchu._
      |import codes.quine.labo.uchu.Card._
      |""".stripMargin,
    Compile / console / scalacOptions -= "-Wunused",
    Test / console / scalacOptions -= "-Wunused",
    // Set URL mapping of scala standard API for Scaladoc.
    apiMappings ++= scalaInstance.value.libraryJars
      .filter(file => file.getName.startsWith("scala-library") && file.getName.endsWith(".jar"))
      .map(_ -> url(s"http://www.scala-lang.org/api/${scalaVersion.value}/"))
      .toMap,
    // Settings for test:
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.22" % Test,
    testFrameworks += new TestFramework("munit.Framework"),
    doctestTestFramework := DoctestTestFramework.Munit,
    doctestMarkdownEnabled := true
  )

lazy val cats = project
  .in(file("modules/uchu-cats"))
  .settings(
    name := "uchu-cats",
    console / initialCommands := """
      |import cats._
      |import cats.data._
      |
      |import codes.quine.labo.uchu._
      |import codes.quine.labo.uchu.Card._
      |import codes.quine.labo.uchu.cats._
      |import codes.quine.labo.uchu.cats.instances._
      |""".stripMargin,
    Compile / console / scalacOptions -= "-Wunused",
    Test / console / scalacOptions -= "-Wunused",
    // Set URL mapping of scala standard API for Scaladoc.
    apiMappings ++= scalaInstance.value.libraryJars
      .filter(file => file.getName.startsWith("scala-library") && file.getName.endsWith(".jar"))
      .map(_ -> url(s"http://www.scala-lang.org/api/${scalaVersion.value}/"))
      .toMap,
    // Dependencies:
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.4.2",
    // Settings for test:
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.22" % Test,
    libraryDependencies += "org.typelevel" %% "cats-laws" % "2.4.2" % Test,
    libraryDependencies += "org.typelevel" %% "discipline-munit" % "1.0.6" % Test,
    testFrameworks += new TestFramework("munit.Framework"),
    doctestTestFramework := DoctestTestFramework.Munit,
    doctestMarkdownEnabled := true
  )
  .dependsOn(core)
