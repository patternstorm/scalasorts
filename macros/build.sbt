name := "macros"

version := "0.1"

scalaVersion := "2.12.6"
resolvers += Resolver.sonatypeRepo("snapshots")
resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies += scalaVersion("org.scala-lang" % "scala-reflect" % _).value
libraryDependencies += "org.scalameta" %% "scalameta" % "1.8.0"
libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
scalacOptions += "-Xplugin-require:macroparadise"


addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M11" cross CrossVersion.full)
