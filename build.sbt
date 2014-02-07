import AssemblyKeys._

name          := "Mellite"

version       := "0.6.0"

organization  := "de.sciss"

homepage      := Some(url("https://github.com/Sciss/" + name.value))

description   := "An application based on SoundProcesses"

licenses      := Seq("GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))

scalaVersion  := "2.10.3"

libraryDependencies ++= Seq(
  "de.sciss" %% "soundprocesses"     % "2.1.1+",
  "de.sciss" %% "scalacolliderswing" % "1.13.+",
  "de.sciss" %% "lucrestm-bdb"       % "2.0.1+",
  "de.sciss" %% "audiowidgets-app"   % "1.4.+",
  "de.sciss" %% "desktop-mac"        % "0.4.+",   // TODO: should be only added on OS X platforms
  "de.sciss" %% "sonogramoverview"   % "1.7.+",
  "de.sciss" %% "treetable-scala"    % "1.3.4+",
  "de.sciss" %% "fscapejobs"         % "1.4.+",
  "de.sciss" %% "strugatzki"         % "2.3.+"
)

retrieveManaged := true

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

scalacOptions += "-no-specialization"

// scalacOptions ++= Seq("-Xelide-below", "INFO")

initialCommands in console :=
  """import de.sciss.mellite._
    |import de.sciss.indeterminus._""".stripMargin

fork in run := true  // required for shutdown hook, and also the scheduled thread pool, it seems

// ---- publishing ----

publishMavenStyle := true

publishTo :=
  Some(if (version.value endsWith "-SNAPSHOT")
    "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  else
    "Sonatype Releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
  )

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := { val n = name.value
<scm>
  <url>git@github.com:Sciss/{n}.git</url>
  <connection>scm:git:git@github.com:Sciss/{n}.git</connection>
</scm>
<developers>
   <developer>
      <id>sciss</id>
      <name>Hanns Holger Rutz</name>
      <url>http://www.sciss.de</url>
   </developer>
</developers>
}


// ---- packaging ----

seq(appbundle.settings: _*)

appbundle.icon      := Some(file("icons") / "application.png")

appbundle.target    := baseDirectory.value

appbundle.signature := "Ttm "

appbundle.javaOptions ++= Seq("-XX:+CMSClassUnloadingEnabled", "-XX:+UseConcMarkSweepGC", "-XX:MaxPermSize=128m")

appbundle.documents += appbundle.Document(
  name       = "Mellite Document",
  role       = appbundle.Document.Editor,
  icon       = Some(file("icons") / "document.png"),
  extensions = Seq("mllt"),
  isPackage  = true
)

assemblySettings

target in assembly := baseDirectory.value

jarName in assembly := s"${name.value}.jar"

