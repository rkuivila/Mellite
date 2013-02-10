name := "Mellite"

version := "0.1.0-SNAPSHOT"

organization := "de.sciss"

homepage := Some(url("https://github.com/Sciss/Mellite"))

description := "An application based on SoundProcesses"

licenses := Seq("GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))

scalaVersion := "2.10.+"

libraryDependencies ++= Seq(
   "de.sciss" %% "soundprocesses" % "1.4.+",
//   "de.sciss" %% "audiowidgets-swing" % "1.1.+",
   "de.sciss" %% "scalacolliderswing" % "1.3.1+",
   "de.sciss" %% "scalainterpreterpane" % "1.3.1-SNAPSHOT",
   "de.sciss" %% "lucrestm-bdb" % "1.7.+",
//   "de.sciss" %% "swingtree" % "1.3.+"
   "com.github.benhutchison" % "scalaswingcontrib" % "1.5-SNAPSHOT"
)

retrieveManaged := true

scalacOptions ++= Seq("-deprecation", "-unchecked", "-no-specialization", "-feature")   // "-Xelide-below", "INFO"

fork in run := true  // required for shutdown hook, and also the scheduled thread pool, it seems

// ---- packaging ----

seq(appbundle.settings: _*)

appbundle.icon := Some(file("application.png"))

appbundle.target <<= baseDirectory
