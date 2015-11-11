// Comment to get more information during initialization
logLevel := Level.Warn

resolvers ++= Seq(
  "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/",
	"Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/",
	Classpaths.sbtPluginReleases
)

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.6.0")

addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.1.5")

addSbtPlugin("com.typesafe.sbt" % "sbt-scalariform" % "1.3.0")

