name := "Mellite"

version := "0.10-SNAPSHOT"

organization := "de.sciss"

homepage := Some( url( "https://github.com/Sciss/Mellite" ))

description := "An application based on SoundProcesses"

licenses := Seq( "GPL v2+" -> url( "http://www.gnu.org/licenses/gpl-2.0.txt" ))

scalaVersion := "2.9.2"

crossScalaVersions in ThisBuild := Seq( "2.10.0-M6", "2.9.2" )

libraryDependencies ++= Seq(
   "de.sciss" %% "soundprocesses" % "0.40-SNAPSHOT",
   "de.sciss" %% "scalaaudiowidgets" % "0.13"
)

retrieveManaged := true

scalacOptions ++= Seq( "-deprecation", "-unchecked", "-no-specialization" )   // "-Xelide-below", "INFO"

fork in run := true  // required for shutdown hook, and also the scheduled thread pool, it seems

// ---- packaging ----

seq( appbundle.settings: _* )

appbundle.icon := Some( file( "application.png" ))

appbundle.target <<= baseDirectory
