lazy val baseName                   = "Mellite"
lazy val baseNameL                  = baseName.toLowerCase
lazy val fullDescr                  = "A computer music application based on SoundProcesses"
lazy val projectVersion             = "1.5.0-SNAPSHOT"

lazy val loggingEnabled             = true

// ---- core dependencies ----

lazy val soundProcessesVersion      = "2.18.1"
lazy val interpreterPaneVersion     = "1.7.1"
lazy val lucreSTMVersion            = "2.1.1"
lazy val fscapeJobsVersion          = "1.5.0"
lazy val strugatzkiVersion          = "2.9.0"

lazy val bdb = "bdb" // either "bdb" or "bdb6"

// ---- views dependencies ----

lazy val nuagesVersion              = "1.3.0"
lazy val scalaColliderSwingVersion  = "1.25.0"
lazy val lucreSwingVersion          = "0.9.1"
lazy val audioWidgetsVersion        = "1.9.0"
lazy val desktopVersion             = "0.7.0"
lazy val sonogramVersion            = "1.9.0"
lazy val treetableVersion           = "1.3.7"
lazy val raphaelIconsVersion        = "1.0.2"
lazy val pdflitzVersion             = "1.2.1"
lazy val webLaFVersion              = "1.28"

// ----

lazy val commonSettings = Seq(
  version            := projectVersion,
  organization       := "de.sciss",
  homepage           := Some(url("https://github.com/Sciss/" + name.value)),
  licenses           := Seq("GPL v3+" -> url("http://www.gnu.org/licenses/gpl-3.0.txt")),
  scalaVersion       := "2.11.6",
  crossScalaVersions := Seq("2.11.6", "2.10.5"),
  scalacOptions ++= {
    val xs = Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture")
    if (loggingEnabled || isSnapshot.value) xs else xs ++ Seq("-Xelide-below", "INFO")
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

lazy val mellite = project.in(file("."))
  .aggregate(`mellite-core`, `mellite-views`)
  .dependsOn(`mellite-core`, `mellite-views`)
  .settings(commonSettings)
  .settings(
    name := baseName,
    description := fullDescr,
    mainClass in (Compile,run) := Some("de.sciss.mellite.Mellite"),
    publishArtifact in (Compile, packageBin) := false, // there are no binaries
    publishArtifact in (Compile, packageDoc) := false, // there are no javadocs
    publishArtifact in (Compile, packageSrc) := false  // there are no sources
  )

lazy val `mellite-core` = project.in(file("core"))
  .settings(commonSettings)
  .settings(
    name        := s"$baseName-core",
    description := "Core layer for Mellite",
    resolvers += "Oracle Repository" at "http://download.oracle.com/maven", // required for sleepycat
    libraryDependencies ++= Seq(
      "de.sciss" %% "soundprocesses-core"   % soundProcessesVersion,  // computer-music framework
      "de.sciss" %% "scalainterpreterpane"  % interpreterPaneVersion, // REPL
      "de.sciss" %% s"lucrestm-$bdb"        % lucreSTMVersion,        // database backend
      "de.sciss" %% "fscapejobs"            % fscapeJobsVersion,      // remote FScape invocation
      "de.sciss" %% "strugatzki"            % strugatzkiVersion       // feature extraction
    ),
    initialCommands in console := "import de.sciss.mellite._"
  )

lazy val `mellite-views` = project.in(file("views")).dependsOn(`mellite-core`)
  .settings(commonSettings)
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name        := s"$baseName-views",
    description := fullDescr,
    libraryDependencies ++= Seq(
      "de.sciss" %% "soundprocesses-views"            % soundProcessesVersion,      // computer-music framework
      "de.sciss" %% "soundprocesses-compiler"         % soundProcessesVersion,      // computer-music framework
      "de.sciss" %% "wolkenpumpe"                     % nuagesVersion,              // live improv
      "de.sciss" %% "scalacolliderswing-interpreter"  % scalaColliderSwingVersion,  // REPL
      "de.sciss" %% "lucreswing"                      % lucreSwingVersion,          // reactive Swing components
      "de.sciss" %% "audiowidgets-app"                % audioWidgetsVersion,        // audio application widgets
      "de.sciss" %% "desktop-mac"                     % desktopVersion,             // desktop framework; TODO: should be only added on OS X platforms
      "de.sciss" %% "sonogramoverview"                % sonogramVersion,            // sonogram component
      "de.sciss" %% "treetable-scala"                 % treetableVersion,           // tree-table component
      "de.sciss" %% "raphael-icons"                   % raphaelIconsVersion,        // icon set
      "de.sciss" %% "pdflitz"                         % pdflitzVersion,             // PDF export
      "de.sciss" %  "weblaf"                          % webLaFVersion               // Swing look-and-feel
    ),
    mainClass in (Compile,run) := Some("de.sciss.mellite.Mellite"),
    initialCommands in console :=
      """import de.sciss.mellite._""".stripMargin,
    fork in run := true,  // required for shutdown hook, and also the scheduled thread pool, it seems
    // ---- build-info ----
    // sourceGenerators in Compile <+= buildInfo,
    buildInfoKeys := Seq("name" -> baseName /* name */, organization, version, scalaVersion, description,
      BuildInfoKey.map(homepage) { case (k, opt) => k -> opt.get },
      BuildInfoKey.map(licenses) { case (_, Seq( (lic, _) )) => "license" -> lic }
    ),
    buildInfoPackage := "de.sciss.mellite",
    // ---- packaging ----
    //    appbundle.icon      := Some(baseDirectory.value / ".." / "icons" / "application.png"),
    //    appbundle.target    := baseDirectory.value / "..",
    //    appbundle.signature := "Ttm ",
    //    appbundle.javaOptions ++= Seq("-Xms2048m", "-Xmx2048m", "-XX:PermSize=256m", "-XX:MaxPermSize=512m"),
    //    appbundle.documents += appbundle.Document(
    //      name       = "Mellite Document",
    //      role       = appbundle.Document.Editor,
    //      icon       = Some(baseDirectory.value / ".." / "icons" / "document.png"),
    //      extensions = Seq("mllt"),
    //      isPackage  = true
    //    ),
    target in assembly := baseDirectory.value / "..",
    assemblyJarName in assembly := s"$baseName.jar"
  )
