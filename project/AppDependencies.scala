import sbt._

object AppDependencies {

  val compile = Seq(
    "uk.gov.hmrc"                   %% "bootstrap-backend-play-28"  % "7.11.0",
    "uk.gov.hmrc.mongo"             %% "hmrc-mongo-play-28"         % "0.73.0",
    "com.github.java-json-tools"    %  "json-schema-validator"      % "2.2.14",
    "com.fasterxml.jackson.module"  %% "jackson-module-scala"       % "2.13.4"
  )

  val test = Seq(
    "uk.gov.hmrc"             %% "bootstrap-test-play-28"     % "7.11.0"                 % "test, it",
    "de.flapdoodle.embed"     %  "de.flapdoodle.embed.mongo"  % "3.5.1"                 % Test,
    "uk.gov.hmrc.mongo"       %% "hmrc-mongo-test-play-28"    % "0.73.0"                % Test,
    "com.vladsch.flexmark"    %  "flexmark-all"               % "0.62.2"                % "test, it",
    "com.github.tomakehurst"  %  "wiremock-jre8"              % "2.35.0"                % Test,
    "org.scalatest"           %% "scalatest"                  % "3.2.14"                % Test,
    "org.scalatestplus.play"  %% "scalatestplus-play"         % "5.1.0"                 % Test,
    "org.scalatestplus"       %% "mockito-4-6"                % "3.2.14.0"              % Test,
    "org.scalatestplus"       %% "scalacheck-1-17"            % "3.2.14.0"              % Test
  )

  def apply(): Seq[ModuleID] = compile ++ test
}
