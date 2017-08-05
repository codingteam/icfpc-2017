import com.github.retronym.SbtOneJar._

name := "codingteam-icfpc2017"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.2"

mainClass in(Compile, run) := Some("org.codingteam.icfpc2017.AppEntry")

fork := true

oneJarSettings

libraryDependencies += "org.scala-graph" %% "graph-core" % "1.11.5"

libraryDependencies += "org.json4s" %% "json4s-jackson" % "3.5.3"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.5.3"

libraryDependencies += "net.ruippeixotog" %% "scala-scraper" % "2.0.0"

scalacOptions += "-opt:l:classpath"

scalacOptions += "-deprecation"

