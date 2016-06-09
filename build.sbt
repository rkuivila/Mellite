import com.typesafe.sbt.packager.linux.LinuxPackageMapping

lazy val baseName                   = "Mellite"
lazy val baseNameL                  = baseName.toLowerCase
lazy val appDescription             = "A computer music application based on SoundProcesses"
lazy val projectVersion             = "2.2.0-SNAPSHOT"

lazy val loggingEnabled             = true

lazy val authorName                 = "Hanns Holger Rutz"
lazy val authorEMail                = "contact@sciss.de"

// ---- core dependencies ----

lazy val soundProcessesVersion      = "3.5.2-SNAPSHOT"
lazy val interpreterPaneVersion     = "1.7.3"
lazy val syntaxPaneVersion          = "1.1.5"
lazy val scalaColliderUGenVersion   = "1.14.1"
lazy val lucreVersion               = "3.3.1"
lazy val fscapeJobsVersion          = "1.5.0"
lazy val strugatzkiVersion          = "2.11.0"
lazy val equalVersion               = "0.1.1"
lazy val scalaOSCVersion            = "1.1.5"
lazy val playJSONVersion            = "0.4.0"

lazy val bdb = "bdb" // either "bdb" or "bdb6"

// ---- views dependencies ----

lazy val nuagesVersion              = "2.7.0"
lazy val scalaColliderSwingVersion  = "1.29.0"
lazy val lucreSwingVersion          = "1.4.0"
lazy val swingPlusVersion           = "0.2.1"
lazy val spanVersion                = "1.3.1"
lazy val audioWidgetsVersion        = "1.10.0"
lazy val desktopVersion             = "0.7.2"
lazy val sonogramVersion            = "1.9.0"
lazy val raphaelIconsVersion        = "1.0.3"
lazy val pdflitzVersion             = "1.2.1"
lazy val subminVersion              = "0.2.1"

// ---- app packaging ----

lazy val appMainClass               = Some("de.sciss.mellite.Mellite")
def appName                         = baseName
def appNameL                        = baseNameL

// ---- common ----

lazy val commonSettings = Seq(
  version            := projectVersion,
  organization       := "de.sciss",
  homepage           := Some(url(s"https://github.com/Sciss/$baseName")),
  licenses           := Seq("GNU General Public License v3+" -> url("http://www.gnu.org/licenses/gpl-3.0.txt")),
  scalaVersion       := "2.11.8",
  crossScalaVersions := Seq("2.11.8", "2.10.6"),
  scalacOptions ++= {
    val xs = Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture")
    if (loggingEnabled || isSnapshot.value) xs else xs ++ Seq("-Xelide-below", "INFO")
  },
  javacOptions ++= Seq("-source", "1.6", "-target", "1.6"),
  resolvers += "Typesafe Maven Repository" at "http://repo.typesafe.com/typesafe/maven-releases/", // https://stackoverflow.com/questions/23979577
  // resolvers += "Typesafe Simple Repository" at "http://repo.typesafe.com/typesafe/simple/maven-releases/", // https://stackoverflow.com/questions/20497271
  aggregate in assembly := false,
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

// ---- packaging ----

//////////////// universal (directory) installer
lazy val pkgUniversalSettings = Seq(
  executableScriptName /* in Universal */ := appNameL,
  // NOTE: doesn't work on Windows, where we have to
  // provide manual file `MELLITE_config.txt` instead!
  javaOptions in Universal ++= Seq(
    // -J params will be added as jvm parameters
    "-J-Xmx1024m"
    // others will be added as app parameters
    // "-Dproperty=true",
  ),
  // Since our class path is very very long,
  // we use instead the wild-card, supported
  // by Java 6+. In the packaged script this
  // results in something like `java -cp "../lib/*" ...`.
  // NOTE: `in Universal` does not work. It therefore
  // also affects debian package building :-/
  // We need this settings for Windows.
  scriptClasspath /* in Universal */ := Seq("*")
)

//////////////// debian installer
lazy val pkgDebianSettings = Seq(
  name                      in Debian := appName,
  packageName               in Debian := appNameL,
  name                      in Linux  := appName,
  packageName               in Linux  := appNameL,
  packageSummary            in Debian := appDescription,
  mainClass                 in Debian := appMainClass,
  maintainer                in Debian := s"$authorName <$authorEMail>",
  debianPackageDependencies in Debian += "java7-runtime",
  packageDescription        in Debian :=
    """Mellite is a computer music environment,
      | a desktop application based on SoundProcesses.
      | It manages workspaces of musical objects, including
      | sound processes, timelines, code fragments, or
      | live improvisation sets.
      |""".stripMargin,
  // include all files in src/debian in the installed base directory
  linuxPackageMappings      in Debian ++= {
    val n     = (name            in Debian).value.toLowerCase
    val dir   = (sourceDirectory in Debian).value / "debian"
    val f1    = (dir * "*").filter(_.isFile).get  // direct child files inside `debian` folder
    val f2    = ((dir / "doc") * "*").get
    //
    def readOnly(in: LinuxPackageMapping) =
      in.withUser ("root")
        .withGroup("root")
        .withPerms("0644")  // http://help.unc.edu/help/how-to-use-unix-and-linux-file-permissions/
    //
    val aux   = f1.map { fIn => packageMapping(fIn -> s"/usr/share/$n/${fIn.name}") }
    val doc   = f2.map { fIn => packageMapping(fIn -> s"/usr/share/doc/$n/${fIn.name}") }
    (aux ++ doc).map(readOnly)
  }
)

// ---- projects ----

lazy val root = Project(id = baseNameL, base = file("."))
  .aggregate(core, views)
  .dependsOn(core, views)
  .enablePlugins(JavaAppPackaging, DebianPlugin)
  .settings(commonSettings)
  .settings(
    name                       := baseName,
    description                := appDescription,
    mainClass in Compile       := appMainClass, // ! cf. https://stackoverflow.com/questions/23664963
    publishArtifact in (Compile, packageBin) := false, // there are no binaries
    publishArtifact in (Compile, packageDoc) := false, // there are no javadocs
    publishArtifact in (Compile, packageSrc) := false,  // there are no sources
    mainClass             in assembly := appMainClass,
    target                in assembly := baseDirectory.value,
    assemblyJarName       in assembly := s"$baseName.jar",
    assemblyMergeStrategy in assembly := {
      case PathList("org", "xmlpull", xs @ _*) => MergeStrategy.first
      case x =>
        val oldStrategy = (assemblyMergeStrategy in assembly).value
        oldStrategy(x)
    }
  )
  .settings(pkgUniversalSettings)
  .settings(useNativeZip) // cf. https://github.com/sbt/sbt-native-packager/issues/334
  .settings(pkgDebianSettings)

lazy val core = Project(id = s"$baseNameL-core", base = file("core"))
  .settings(commonSettings)
  .settings(
    name        := s"$baseName-core",
    description := "Core layer for Mellite",
    resolvers += "Oracle Repository" at "http://download.oracle.com/maven", // required for sleepycat
    libraryDependencies ++= Seq(
      "de.sciss"        %% "soundprocesses-core"      % soundProcessesVersion,    // computer-music framework
      "de.sciss"        %% "scalainterpreterpane"     % interpreterPaneVersion,   // REPL
      "de.sciss"        %  "syntaxpane"               % syntaxPaneVersion,        // REPL view
      "de.sciss"        %% "scalacolliderugens-api"   % scalaColliderUGenVersion, // need latest version
      "de.sciss"        %  "scalacolliderugens-spec"  % scalaColliderUGenVersion, // meta data
      "de.sciss"        %% "lucre-core"               % lucreVersion,
      "de.sciss"        %% "lucre-confluent"          % lucreVersion,
      "de.sciss"        %% s"lucre-$bdb"              % lucreVersion,             // database backend
      "de.sciss"        %% "fscapejobs"               % fscapeJobsVersion,        // remote FScape invocation
      "de.sciss"        %% "strugatzki"               % strugatzkiVersion,        // feature extraction
      "de.sciss"        %% "span"                     % spanVersion,              // makes sbt happy :-E
      "de.sciss"        %% "equal"                    % equalVersion,             // type-safe equals
      "de.sciss"        %% "scalaosc"                 % scalaOSCVersion,          // important fixes
      "de.sciss"        %% "play-json-sealed"         % playJSONVersion
    ),
    initialCommands in console := "import de.sciss.mellite._"
  )

lazy val views = Project(id = s"$baseNameL-views", base = file("views"))
  .dependsOn(core)
  .enablePlugins(BuildInfoPlugin)
  .settings(commonSettings)
  .settings(
    name        := s"$baseName-views",
    description := appDescription,
    libraryDependencies ++= Seq(
      "de.sciss" %% "soundprocesses-views"            % soundProcessesVersion,      // computer-music framework
      "de.sciss" %% "soundprocesses-compiler"         % soundProcessesVersion,      // computer-music framework
      "de.sciss" %% "wolkenpumpe"                     % nuagesVersion,              // live improv
      "de.sciss" %% "scalacolliderswing-interpreter"  % scalaColliderSwingVersion,  // REPL
      "de.sciss" %% "lucreswing"                      % lucreSwingVersion,          // reactive Swing components
      "de.sciss" %% "swingplus"                       % swingPlusVersion,           // newest version: color-chooser
      "de.sciss" %% "span"                            % spanVersion,
      "de.sciss" %% "audiowidgets-swing"              % audioWidgetsVersion,        // audio application widgets
      "de.sciss" %% "audiowidgets-app"                % audioWidgetsVersion,        // audio application widgets
      "de.sciss" %% "desktop"                         % desktopVersion,
      "de.sciss" %% "desktop-mac"                     % desktopVersion,             // desktop framework; TODO: should be only added on OS X platforms
      "de.sciss" %% "sonogramoverview"                % sonogramVersion,            // sonogram component
      "de.sciss" %% "raphael-icons"                   % raphaelIconsVersion,        // icon set
      "de.sciss" %% "pdflitz"                         % pdflitzVersion,             // PDF export
      "de.sciss" %  "submin"                          % subminVersion               // dark skin
    ),
    mainClass in (Compile,run) := appMainClass,
    initialCommands in console :=
      """import de.sciss.mellite._""".stripMargin,
    fork in run := true,  // required for shutdown hook, and also the scheduled thread pool, it seems
    // ---- build-info ----
    buildInfoKeys := Seq("name" -> baseName /* name */, organization, version, scalaVersion, description,
      BuildInfoKey.map(homepage) { case (k, opt) => k -> opt.get },
      BuildInfoKey.map(licenses) { case (_, Seq( (lic, _) )) => "license" -> lic }
    ),
    buildInfoPackage := "de.sciss.mellite"
  )
