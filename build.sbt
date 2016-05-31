sbtPlugin := true

organization := "net.ground5hark.sbt"

name := "sbt-concat"

scalaVersion := "2.10.6"

addSbtPlugin("com.typesafe.sbt" %% "sbt-web" % "1.4.0")

scriptedSettings

scriptedLaunchOpts ++= Seq(
  "-Xmx1024M",
  "-XX:MaxPermSize=256M",
  s"-Dproject.version=${version.value}"
)

