import sbt._
import Keys._

lazy val commonSettings = Seq(
  name := "play-multipart-mixed",
  version := "1.0.0",
  organization := "com.socialorra",
  scalaVersion := "2.11.7",
  resolvers ++= Seq(Resolver.mavenLocal,
    "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/",
    "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/",
    "Typesafe Snapshots" at "http://repo.typesafe.com/typesafe/snapshots/",
    "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
    "Atlassian Releases" at "https://maven.atlassian.com/public/")
)

lazy val deps = Seq(
  "com.typesafe.play" %% "play" % "2.4.3",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test",
  "org.scalatestplus" %% "play" % "1.4.0-M3" % "test"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(libraryDependencies ++= deps).
  settings(Format.settings) 

