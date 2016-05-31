sbtPlugin := true

organization := "com.26lights"

name := "sbt-concat"

scalaVersion := "2.10.6"

scalacOptions += "-feature"

addSbtPlugin("com.typesafe.sbt" %% "sbt-web" % "1.4.0")

scriptedSettings

scriptedLaunchOpts ++= Seq(
  "-Xmx1024M",
  "-XX:MaxPermSize=256M",
  s"-Dproject.version=${version.value}"
)

