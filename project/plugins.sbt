addSbtPlugin("com.eed3si9n"     % "sbt-assembly"        % "0.14.4")    // cross-platform standalone
addSbtPlugin("com.eed3si9n"     % "sbt-buildinfo"       % "0.6.1")     // meta-data such as project and Scala version
addSbtPlugin("com.typesafe"     % "sbt-mima-plugin"     % "0.1.14")    // binary compatibility testing

// N.B. 1.1.1 has some problem with Java 6
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.0.5")  // release standalone binaries
