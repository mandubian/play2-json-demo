import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

    val appName         = "json-coast-to-coast"
    val appVersion      = "0.1-SNAPSHOT"
    val buildScalaVersion = "2.10.0"

    val appDependencies = Seq(
      // Add your project dependencies here,
      "play.modules.reactivemongo" % "play2-reactivemongo_2.10.0" % "0.1-SNAPSHOT" exclude("play", "play_2.10-RC2")
    )

    val main = play.Project(appName, appVersion, appDependencies).settings(
      //resolvers += ("mandubian-mvn snapshots" at "https://github.com/mandubian/mandubian-mvn/raw/master/snapshots")
      scalaVersion := buildScalaVersion,
      resolvers += "sgodbillon" at "https://bitbucket.org/sgodbillon/repository/raw/master/snapshots/"
    )

}
