name := "xke-kmeans"

version := "1.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats" % "0.8.0",
  "ch.qos.logback"    %  "logback-classic" % "1.1.3",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)

// Assembly settings
//mainClass in Global := Some("fr.xebia.streams.GroupedSource")

fork in run := true
