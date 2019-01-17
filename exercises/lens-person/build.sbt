scalaVersion := "2.12.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

val monocleVersion = "1.5.0"

libraryDependencies ++= Seq(
  "com.github.julien-truffaut" %%  "monocle-core"  % monocleVersion,
  "com.github.julien-truffaut" %%  "monocle-macro" % monocleVersion)

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.6"

