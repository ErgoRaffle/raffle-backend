import sbt.Keys.{libraryDependencies, resolvers}

name := "ErgoRaffle"
organization := "ErgoRaffle"

version := "1.0.0-beta"

lazy val root = (project in file(".")).enablePlugins(PlayScala).settings(
  scalaVersion := "2.12.10",
  resolvers ++= Seq("Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
    "SonaType" at "https://oss.sonatype.org/content/groups/public",
    "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  ),
  libraryDependencies ++= Seq(ehcache, ws, specs2%Test, guice),
  libraryDependencies ++= Seq(
    guice,
    "org.scalaj" %% "scalaj-http" % "2.4.2",
    "com.dripower" %% "play-circe" % "2712.0",
    "org.ergoplatform" %% "ergo-appkit" % "develop-dd40e4e5-SNAPSHOT",
    "org.scorexfoundation" %% "scrypto" % "2.1.10",
    "com.typesafe.play" %% "play-slick" % "4.0.0",
    "com.typesafe.play" %% "play-slick-evolutions" % "4.0.0",
    "com.h2database" % "h2" % "1.4.200",
    "io.kinoplan" % "emailaddress-play-json_2.12" % "0.1.0"
  )

)

fullClasspath in assembly += Attributed.blank(PlayKeys.playPackageAssets.value)

val testingDependencies = Seq(
  "org.scalatest" %% "scalatest" % "3.0.8" % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.+" % "test",
  "org.scalatestplus.play" %% "scalatestplus-play" % "5.0.0" % "test"
)


lazy val testSettings = Seq(
  libraryDependencies ++= testingDependencies,
  parallelExecution in Test := false,
  baseDirectory in Test := file("."),
  publishArtifact in Test := true,
  publishArtifact in(Test, packageSrc) := true,
  publishArtifact in(Test, packageDoc) := false,
  test in assembly := {})


assemblyMergeStrategy in assembly := {
  case PathList("reference.conf") => MergeStrategy.concat
  case manifest if manifest.contains("MANIFEST.MF") => MergeStrategy.discard
  case manifest if manifest.contains("module-info.class") => MergeStrategy.discard
  case referenceOverrides if referenceOverrides.contains("reference-overrides.conf") => MergeStrategy.concat
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}

assemblyJarName in assembly := s"${name.value}-${version.value}.jar"

