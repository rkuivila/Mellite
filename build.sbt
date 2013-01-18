name := "Mellite"

version := "0.1.0-SNAPSHOT"

organization := "de.sciss"

homepage := Some( url( "https://github.com/Sciss/Mellite" ))

description := "An application based on SoundProcesses"

licenses := Seq( "GPL v2+" -> url( "http://www.gnu.org/licenses/gpl-2.0.txt" ))

scalaVersion := "2.10.0"

crossScalaVersions in ThisBuild := Seq( "2.10.0", "2.9.2" )

libraryDependencies ++= Seq(
   "de.sciss" %% "soundprocesses" % "1.4.+",
   "de.sciss" %% "audiowidgets-swing" % "1.1.+",
   "de.sciss" %% "scalainterpreterpane" % "1.3.+",
   "de.sciss" %% "lucrestm-bdb" % "1.6.+"
)

retrieveManaged := true

scalacOptions ++= Seq( "-deprecation", "-unchecked", "-no-specialization" )   // "-Xelide-below", "INFO"

fork in run := true  // required for shutdown hook, and also the scheduled thread pool, it seems

// ---- packaging ----

seq( appbundle.settings: _* )

appbundle.icon := Some( file( "application.png" ))

appbundle.target <<= baseDirectory
