enablePlugins(org.nlogo.build.NetLogoExtension)

scalaVersion := "2.12.2"

scalaSource in Compile := { baseDirectory.value  / "src" }

javaSource in Compile  := { baseDirectory.value / "src" }

scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xlint", "-Xfatal-warnings",
  "-encoding", "us-ascii")

javacOptions ++= Seq("-g", "-deprecation", "-Xlint:all", "-encoding", "us-ascii")

name := "k-means"

netLogoVersion      := "6.0.2"

netLogoClassManager := "org.nlogo.extensions.kmeans.KMeansExtension"

netLogoExtName      := "kmeans"

netLogoZipSources   := false

//libraryDependencies += "net.sf.jung" % "jung-algorithms" % "2.1.1"
