import com.jsuereth.sbtpgp.PgpKeys.publishSigned
import sbt.Keys.*
import sbt.*
import xerial.sbt.Sonatype.*

ThisBuild / scalaVersion := "3.3.1"
ThisBuild / scalacOptions ++= Seq(
				"-feature", 
				"-unchecked")
ThisBuild / organization        := "net.team2xh"
publishTo                       := sonatypePublishTo.value
publishMavenStyle               := true
ThisBuild / sonatypeProfileName := "net.team2xh"
ThisBuild / sonatypeProjectHosting := Some(
  GitHubHosting(user = "Tenchi2xh", repository = "Scurses", email = "tenchi@team2xh.net")
)
ThisBuild / developers := List(
  Developer(id = "tenchi", name = "Hamza Haiken", email = "tenchi@team2xh.net", url = url("http://tenchi.me"))
)
ThisBuild / licenses := Seq("MIT" -> url("https://github.com/Tenchi2xh/Scurses/blob/master/LICENSE"))

// This is causing problems with env variables being passed in, see
// https://github.com/sbt/sbt/issues/6468
ThisBuild / githubWorkflowUseSbtThinClient      := false
ThisBuild / githubWorkflowPublishTargetBranches := Seq()

lazy val root = (project in file("."))
  .aggregate(scurses, onions)
  .settings(
    publish / skip       := true,
    publishLocal / skip  := true,
    publishSigned / skip := true
  )

lazy val scurses = (project in file("scurses"))
  .settings(
    name                                 := "scurses",
    version                              := "2.0.0",
    libraryDependencies += "com.lihaoyi" %% "fastparse" % "3.0.1",
    Compile / run / mainClass            := Some("net.team2xh.scurses.examples.GameOfLife")
  )

lazy val onions = (project in file("onions"))
  .dependsOn(scurses)
  .settings(
    name                      := "onions",
    version                   := "2.0.0",
    Compile / run / mainClass := Some("net.team2xh.onions.examples.ExampleUI")
  )
