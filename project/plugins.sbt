addSbtPlugin("org.xerial.sbt" % "sbt-sonatype"       % "3.9.7")
addSbtPlugin("com.github.sbt" % "sbt-pgp"            % "2.1.2")
addSbtPlugin("com.codecommit" % "sbt-github-actions" % "0.12.0")
addSbtPlugin("org.scalameta"  % "sbt-scalafmt"       % "2.4.3")
addSbtPlugin("ch.epfl.scala"  % "sbt-scala3-migrate" % "0.4.6")

logLevel := Level.Warn
