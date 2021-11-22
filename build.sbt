name := "chronos"
version := "0.1"
scalaVersion := "3.1.0"

libraryDependencies := Seq (
  "org.typelevel" %% "cats-core" % "2.6.1",
  "org.typelevel" %% "cats-effect" % "3.2.8",
  ("com.typesafe.akka" %% "akka-actor-typed" % "2.6.17")
    .cross(CrossVersion.for3Use2_13)
)