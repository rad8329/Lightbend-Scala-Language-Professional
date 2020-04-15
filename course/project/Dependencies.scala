import sbt._

object Version {
  val scalaVer     = "2.13.1"
  val scalaTest    = "3.1.0"
  val playJson     = "2.8.1"
}

object Library {
  val scalaTest    = "org.scalatest"          %% "scalatest"                % Version.scalaTest
  val playJson     = "com.typesafe.play"      %% "play-json"                % Version.playJson
}

object Dependencies {

  import Library._

  val dependencies = List(
    playJson,
    scalaTest % "test"
  )
}
