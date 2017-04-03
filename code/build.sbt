organization      in ThisBuild := "io.underscore"
version           in ThisBuild := "0.1.0"

scalaOrganization in ThisBuild := "org.typelevel"
scalaVersion      in ThisBuild := "2.12.1"

lazy val macros = project.in(file("macros"))
  .settings(
    name                := "macros",
    libraryDependencies ++= Seq(
      "org.scala-lang" %  "scala-reflect" % scalaVersion.value,
      "org.typelevel"  %% "macro-compat"  % "1.1.1",
      "org.scalatest"  %% "scalatest"     % "3.0.1" % Test
    ),
    addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)
  )

lazy val shapeless = project.in(file("shapeless"))
  .settings(
    name                := "shapeless",
    libraryDependencies ++= Seq(
      "com.chuusai"   %% "shapeless" % "2.3.2",
      "org.scalatest" %% "scalatest" % "3.0.1" % Test
    ),
    scalacOptions += "-Yliteral-types"
  )

lazy val root = project.in(file("."))
  .aggregate(macros, shapeless)
