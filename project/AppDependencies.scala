import sbt._

object AppDependencies {

  val compile = Seq(
    "uk.gov.hmrc"                   %% "bootstrap-backend-play-28"  % "7.8.0",
    "uk.gov.hmrc.mongo"             %% "hmrc-mongo-play-28"         % "0.73.0",
    "com.github.java-json-tools"    %  "json-schema-validator"      % "2.2.14",
    "com.fasterxml.jackson.module"  %% "jackson-module-scala"       % "2.13.4"
  )

  val test = Seq(
    "uk.gov.hmrc"             %% "bootstrap-test-play-28"     % "7.8.0"                 % "test, it",
    "de.flapdoodle.embed"     %  "de.flapdoodle.embed.mongo"  % "3.5.1"                 % Test,
    "uk.gov.hmrc.mongo"       %% "hmrc-mongo-test-play-28"    % "0.73.0"                % Test,
    "com.vladsch.flexmark"    %  "flexmark-all"               % "0.62.2"                % "test, it",
    "com.github.tomakehurst"  %  "wiremock"                   % "2.27.2"                % "test",
    "com.github.tomakehurst"  %  "wiremock-jre8"              % "2.34.0"                % "test",
    "org.scalatest"           %% "scalatest"                  % "3.2.14"                % "test",
    "org.scalatestplus.play"  %% "scalatestplus-play"         % "5.1.0"                 % "test",
    "org.scalatestplus"       %% "mockito-3-4"                % "3.2.10.0"              % "test",
    "org.scalatestplus"       %% "scalacheck-1-15"            % "3.2.11.0"              % "test"
  )

  def apply(): Seq[ModuleID] = compile ++ test
}
