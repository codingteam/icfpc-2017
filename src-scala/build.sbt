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

lazy val deploy: TaskKey[File] = TaskKey("deploy", "Make assembly & copy it to ../deploy directory")

deploy := {
  val deploy = file("../deploy")
  IO.createDirectory(deploy)

  val jarFile = deploy / "codingteam-punter.jar"
  val copyDirs = Seq(file("./src") -> deploy / "src/src")
  copyDirs foreach { case (s, d) =>
    println(s"Copy dir $s to '${d.getCanonicalPath}'...")
    IO.delete(d)
    IO.copyDirectory(s, d, overwrite = true, preserveLastModified = true)
  }

  val copyFiles = Seq(
    assembly.value -> jarFile,
    file("./build.sbt") -> deploy / "src/build.sbt",
    file("./project/plugins.sbt") -> deploy / "src/project/plugins.sbt",
    file("./project/build.properties") -> deploy / "src/project/build.properties"
  )
  copyFiles foreach { case (s, d) =>
    println(s"Copy file $s to '${d.getCanonicalPath}'...")
    IO.delete(d)
    IO.copyFile(s, d, preserveLastModified = true)
  }

  jarFile
}

cleanFiles ++= {
  val deploy = file("../deploy")
  Seq(deploy / "codingteam-punter.jar", deploy / "src")
}
