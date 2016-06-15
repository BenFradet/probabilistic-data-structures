lazy val compilerOptions = Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Xfuture",
  "-Xlint"
)

lazy val root = (project in file("."))
  .settings(
    organization := "com.github.benfradet",
    version := "1.0",
    scalaVersion := "2.11.8",
    name := "probabilistic-data-structures",
    libraryDependencies ++= Seq(),
    scalacOptions ++= compilerOptions
  )
