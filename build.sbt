name := "NimLink"

version := "0.1"

scalaVersion := "2.12.8"

scalacOptions += "-Ypartial-unification"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.6.0",
  "org.typelevel" %% "mouse" % "1.6.0",
  "org.typelevel" %% "cats-collections" % "1.6.0",
)