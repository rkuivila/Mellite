// resolvers += Resolver.url( "sbt-plugin-releases", url( "http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases/" ))( Resolver.ivyStylePatterns )

resolvers ++= Seq(
   "less is" at "http://repo.lessis.me",
   "coda" at "http://repo.codahale.com"
)

// broken thanks to sbt 0.12
// addSbtPlugin( "de.sciss" % "sbt-appbundle" % "0.14" )
