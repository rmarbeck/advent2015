val scala3Version = "3.3.1"


lazy val adventday24 = (project in file(".")).
  settings(
    name := "day24",
    version := "0.9",
    description := "Advent of code : day24",

    scalaVersion := scala3Version,

    scalacOptions := Seq("-unchecked", "-deprecation"),

    libraryDependencies ++= Seq(
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
      "ch.qos.logback" % "logback-classic" % "1.4.11",
      "org.scalameta" %% "munit" % "0.7.29" % Test
    )
  )
