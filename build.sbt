name := "WikiGrabber"

version := "1.0"

scalaVersion := "2.11.8"

resolvers += "mmreleases" at "https://artifactory.mediamath.com/artifactory/libs-release-global"

libraryDependencies += "net.ruippeixotog" %% "scala-scraper" % "1.0.0"
libraryDependencies += "com.lihaoyi" %% "pprint" % "0.4.1"
libraryDependencies += "com.mediamath" %% "scala-json" % "1.0"
    