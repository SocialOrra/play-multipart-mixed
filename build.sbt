import sbt._
import Keys._

lazy val commonSettings = Seq(
  name := "play-multipart-mixed",
  version := "1.0.0",
  organization := "com.socialorra",
  scalaVersion := "2.11.7",
  javacOptions ++= Seq("-source", "1.8", "-target", "1.8", "-Xlint:unchecked", "-encoding", "UTF-8"),
  scalacOptions += "-deprecation",
  pomExtra := {
    <scm>
      <url>https://github.com/socialorra/play-multipart-mixed</url>
      <connection>scm:git:git@github.com:socialorra/play-multipart-mixed.git</connection>
    </scm>
      <developers>
        <developer>
          <id>socialorra</id>
          <name>SocialOrra</name>
          <url>https://socialorra.com</url>
        </developer>
      </developers>
  },
  pomIncludeRepository := { _ => false },
  homepage := Some(url(s"https://github.com/socialorra/play-multipart-mixed")),
  licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.html")),
  resolvers ++= Seq(Resolver.mavenLocal,
    "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/",
    "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/",
    "Typesafe Snapshots" at "http://repo.typesafe.com/typesafe/snapshots/",
    "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
    "Atlassian Releases" at "https://maven.atlassian.com/public/")
)

lazy val deps = Seq(
  "com.typesafe.play" %% "play" % "2.4.3" % "provided",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test",
  "org.scalatestplus" %% "play" % "1.4.0-M3" % "test"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(libraryDependencies ++= deps).
  settings(Format.settings) 

