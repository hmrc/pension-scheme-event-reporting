import sbt._

object AppDependencies {

  private val MongoVersion = "0.74.0"

  val compile = Seq(
    "uk.gov.hmrc"                   %% "bootstrap-backend-play-28"  % "7.13.0",
    "uk.gov.hmrc.mongo"             %% "hmrc-mongo-play-28"         % MongoVersion,
    "com.github.java-json-tools"    %  "json-schema-validator"      % "2.2.14",
    "com.fasterxml.jackson.module"  %% "jackson-module-scala"       % "2.14.2"
  )

  val test = Seq(
    "uk.gov.hmrc"             %% "bootstrap-test-play-28"     % "7.13.0"                 % "test, it",
    "de.flapdoodle.embed"     %  "de.flapdoodle.embed.mongo"  % "3.5.3"                 % Test,
    "uk.gov.hmrc.mongo"       %% "hmrc-mongo-test-play-28"    % MongoVersion           % Test,
    "com.vladsch.flexmark"    %  "flexmark-all"               % "0.64.0"                % "test, it",
    "com.github.tomakehurst"  %  "wiremock-jre8"              % "2.35.0"                % Test,
    "org.scalatest"           %% "scalatest"                  % "3.2.15"                % Test,
    "org.scalatestplus.play"  %% "scalatestplus-play"         % "5.1.0"                 % Test,
    "org.scalatestplus"       %% "mockito-4-6"                % "3.2.15.0"              % Test,
    "org.scalatestplus"       %% "scalacheck-1-17"            % "3.2.15.0"              % Test
  )

  def apply(): Seq[ModuleID] = compile ++ test
}
