name := "play-php"

organization := "com.davegurnell"

version := "0.1"

scalaVersion := "2.11.2"

libraryDependencies ++= Seq(
  "org.scalatest"     %% "scalatest" % "2.2.1" % "test",
  "com.typesafe.play" %% "play-json" % "2.3.4"
)

scalacOptions += "-feature"