name := "Telegram-bot"

version := "0.1"

scalaVersion := "2.12.4"

val scalacFlags = Seq(
  "-encoding", "utf8",
  "-feature",
  "-unchecked",
  "-deprecation",
  "-language:higherKinds",
  "-Xlog-reflective-calls",
  "-Ywarn-value-discard",
  "-Ypartial-unification"
)

scalacOptions in(Compile, compile) ++= scalacFlags

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.1.0"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

libraryDependencies += "info.mukel" %% "telegrambot4s" % "3.0.14"
