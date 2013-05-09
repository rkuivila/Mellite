name := "Mellite"

version := "0.2.0-SNAPSHOT"

organization := "de.sciss"

homepage <<= name { n => Some(url("https://github.com/Sciss/" + n)) }

description := "An application based on SoundProcesses"

licenses := Seq("GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))

scalaVersion := "2.10.1"

libraryDependencies ++= Seq(
  "de.sciss" %% "soundprocesses"     % "1.7.+",
  "de.sciss" %% "scalacolliderswing" % "1.7.+",
  "de.sciss" %% "lucrestm-bdb"       % "2.0.+",
  "de.sciss" %% "desktop"            % "0.3.+",
  "de.sciss" %% "sonogramoverview"   % "1.6.1+",
  "de.sciss" %  "jtreetable"         % "1.1.+"
)

retrieveManaged := true

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

scalacOptions += "-no-specialization"

// scalacOptions ++= Seq("-Xelide-below", "INFO")

fork in run := true  // required for shutdown hook, and also the scheduled thread pool, it seems

// ---- packaging ----

seq(appbundle.settings: _*)

appbundle.icon := Some(file("application.png"))

appbundle.target <<= baseDirectory

appbundle.signature := "Ttm "

appbundle.documents += appbundle.Document(
  name       = "Mellite Document",
  role       = appbundle.Document.Editor,
  icon       = Some(file("document.png")),
  extensions = Seq("mllt"),
  isPackage  = true
)
