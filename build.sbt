name := "scalasorts"

version := "0.1"

scalaVersion := "2.12.6"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
libraryDependencies += "org.scalameta" %% "scalameta" % "1.8.0"
libraryDependencies += "default" %% "macros" % "0.1"

//resolvers += Resolver.sonatypeRepo("snapshots")
//resolvers += Resolver.sonatypeRepo("releases")
addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M11" cross CrossVersion.full)
scalacOptions += "-Xplugin-require:macroparadise"
