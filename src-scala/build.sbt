import com.github.retronym.SbtOneJar._

name := "codingteam-icfpc2017"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.2"

mainClass in(Compile, run) := Some("org.codingteam.icfpc2017.AppEntry")

fork := true

oneJarSettings

libraryDependencies += "org.scala-graph" %% "graph-core" % "1.11.5"

libraryDependencies += "org.json4s" %% "json4s-jackson" % "3.5.3"

