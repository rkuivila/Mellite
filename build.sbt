import AssemblyKeys._

lazy val baseName   = "Mellite"

lazy val baseNameL  = baseName.toLowerCase

lazy val fullDescr  = "A computer music application based on SoundProcesses"

lazy val commonSettings = Project.defaultSettings ++ Seq(
  version            := "0.10.0-SNAPSHOT",
  organization       := "de.sciss",
  homepage           := Some(url("https://github.com/Sciss/" + name.value)),
  licenses           := Seq("GPL v3+" -> url("http://www.gnu.org/licenses/gpl-3.0.txt")),
  scalaVersion       := "2.11.2",
  crossScalaVersions := Seq("2.11.2", "2.10.4"),
  scalacOptions ++= {
    val xs = Seq("-deprecation", "-unchecked", "-feature", "-Xfuture")
    if (version.value endsWith "-SNAPSHOT") xs else xs ++ Seq("-Xelide-below", "INFO")
  },
  javacOptions ++= Seq("-source", "1.6", "-target", "1.6"),
  // ---- publishing ----
  publishMavenStyle := true,
  publishTo := {
    Some(if (isSnapshot.value)
      "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
    else
      "Sonatype Releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
    )
  },
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
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
)

// ---- projects ----

lazy val full = Project(
  id            = baseNameL,
  base          = file("."),
  aggregate     = Seq(core, views),
  dependencies  = Seq(core, views),
  settings      = commonSettings ++ Seq(
    name := baseName,
    description := fullDescr,
    publishArtifact in (Compile, packageBin) := false, // there are no binaries
    publishArtifact in (Compile, packageDoc) := false, // there are no javadocs
    publishArtifact in (Compile, packageSrc) := false  // there are no sources
  )
)

lazy val core: Project = Project(
  id        = s"$baseNameL-core",
  base      = file("core"),
  settings  = commonSettings ++ Seq(
    name        := s"$baseName-core",
    description := "Core layer for Mellite",
    resolvers += "Oracle Repository" at "http://download.oracle.com/maven", // required for sleepycat
    libraryDependencies ++= Seq(
      "de.sciss" %% "scalainterpreterpane"            % "1.6.2",  // REPL
      "de.sciss" %% "soundprocesses"                  % "2.6.0-SNAPSHOT",  // computer-music framework
      "de.sciss" %% "lucrestm-bdb"                    % "2.1.0",  // database backend
      "de.sciss" %% "fscapejobs"                      % "1.4.1",  // remote FScape invocation
      "de.sciss" %% "strugatzki"                      % "2.4.1"   // feature extraction
    ),
    initialCommands in console := "import de.sciss.mellite._"
  )
)

lazy val views = Project(
  id        = s"$baseNameL-views",
  base      = file("views"),
  dependencies = Seq(core),
  settings  = commonSettings ++ assemblySettings ++ appbundle.settings ++ Seq(
    name        := s"$baseName-views",
    description := fullDescr,
    libraryDependencies ++= Seq(
      "de.sciss" %% "scalacolliderswing-interpreter"  % "1.16.0", // REPL
      "de.sciss" %% "lucreswing"                      % "0.4.1",  // reactive Swing components
      "de.sciss" %% "audiowidgets-app"                % "1.6.2",  // audio application widgets
      "de.sciss" %% "desktop-mac"                     % "0.5.4",  // desktop framework; TODO: should be only added on OS X platforms
      "de.sciss" %% "sonogramoverview"                % "1.7.1",  // sonogram component
      "de.sciss" %% "treetable-scala"                 % "1.3.6",  // tree-table component
      "de.sciss" %% "raphael-icons"                   % "1.0.2",  // icon set
      "de.sciss" %% "pdflitz"                         % "1.1.0",  // PDF export
      "de.sciss" %  "weblaf"                          % "1.28"    // Swing look-and-feel
    ),
    mainClass in (Compile,run) := Some("de.sciss.mellite.Mellite"),
    initialCommands in console :=
      """import de.sciss.mellite._
        |import de.sciss.indeterminus._""".stripMargin,
    fork in run := true,  // required for shutdown hook, and also the scheduled thread pool, it seems
    // ---- packaging ----
    appbundle.icon      := Some(baseDirectory.value / ".." / "icons" / "application.png"),
    appbundle.target    := baseDirectory.value / "..",
    appbundle.signature := "Ttm ",
    appbundle.javaOptions ++= Seq("-XX:+CMSClassUnloadingEnabled", "-XX:+UseConcMarkSweepGC", "-XX:MaxPermSize=128m"),
    appbundle.documents += appbundle.Document(
      name       = "Mellite Document",
      role       = appbundle.Document.Editor,
      icon       = Some(baseDirectory.value / ".." / "icons" / "document.png"),
      extensions = Seq("mllt"),
      isPackage  = true
    ),
    target in assembly := baseDirectory.value / "..",
    jarName in assembly := s"$baseName.jar"
  )
)
