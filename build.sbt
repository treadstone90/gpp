name := "gpp"

version := "0.1"

organization := "edu.utexas"

scalaVersion := "2.10.1"

crossPaths := false

retrieveManaged := true

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "SonaType Repo" at "https://oss.sonatype.org/content/repositories/snapshots"



libraryDependencies ++= Seq(
  "org.scalanlp" % "nak" % "1.1.2",
  "org.scalanlp" % "chalk" % "1.1.3-SNAPSHOT",
  "com.typesafe.akka" %% "akka-actor" % "2.1.2",
  "org.rogach" %% "scallop" % "0.8.1",
  "commons-codec" % "commons-codec" % "1.7"
)

