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
ThisBuild / scalafixDependencies += "com.github.vovapolu" %% "scaluzzi" % "0.1.18"

lazy val root = project
  .in(file("."))
  .aggregate(core, cats, shapeless, laws)

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
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.23" % Test,
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
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.5.0",
    // Settings for test:
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.23" % Test,
    testFrameworks += new TestFramework("munit.Framework"),
    doctestTestFramework := DoctestTestFramework.Munit,
    doctestMarkdownEnabled := true
  )
  .dependsOn(core)

lazy val shapeless = project
  .in(file("modules/uchu-shapeless"))
  .settings(
    name := "uchu-shapeless",
    console / initialCommands := """
      |import shapeless._
      |
      |import codes.quine.labo.uchu._
      |import codes.quine.labo.uchu.Card._
      |import codes.quine.labo.uchu.shapeless._
      |""".stripMargin,
    Compile / console / scalacOptions -= "-Wunused",
    Test / console / scalacOptions -= "-Wunused",
    // Set URL mapping of scala standard API for Scaladoc.
    apiMappings ++= scalaInstance.value.libraryJars
      .filter(file => file.getName.startsWith("scala-library") && file.getName.endsWith(".jar"))
      .map(_ -> url(s"http://www.scala-lang.org/api/${scalaVersion.value}/"))
      .toMap,
    // Dependencies:
    libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.3",
    // Settings for test:
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.23" % Test,
    testFrameworks += new TestFramework("munit.Framework"),
    doctestTestFramework := DoctestTestFramework.Munit,
    doctestMarkdownEnabled := true
  )
  .dependsOn(core)

lazy val laws = project
  .in(file("modules/uchu-laws"))
  .settings(
    name := "uchu-laws",
    console / initialCommands := """
      |import codes.quine.labo.uchu._
      |import codes.quine.labo.uchu.Card._
      |import codes.quine.labo.uchu.laws._
      |""".stripMargin,
    Compile / console / scalacOptions -= "-Wunused",
    Test / console / scalacOptions -= "-Wunused",
    // Set URL mapping of scala standard API for Scaladoc.
    apiMappings ++= scalaInstance.value.libraryJars
      .filter(file => file.getName.startsWith("scala-library") && file.getName.endsWith(".jar"))
      .map(_ -> url(s"http://www.scala-lang.org/api/${scalaVersion.value}/"))
      .toMap,
    // Dependencies:
    libraryDependencies += "org.typelevel" %% "cats-kernel" % "2.5.0",
    libraryDependencies += "org.typelevel" %% "discipline-core" % "1.1.4",
    // Settings for test:
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.23" % Test,
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.22" % Test,
    libraryDependencies += "org.typelevel" %% "discipline-munit" % "1.0.7" % Test,
    libraryDependencies += "org.typelevel" %% "cats-laws" % "2.5.0" % Test,
    libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.3" % Test,
    testFrameworks += new TestFramework("munit.Framework"),
    doctestTestFramework := DoctestTestFramework.Munit,
    doctestMarkdownEnabled := true
  )
  .dependsOn(core, cats, shapeless % Test)
