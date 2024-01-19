import sbt._

object AppDependencies {

  private val mongoVersion = "1.7.0"
  private val bootstrapVersion = "8.4.0"
  val compile = Seq(
    "uk.gov.hmrc"                   %% "bootstrap-backend-play-30"  % bootstrapVersion,
    "uk.gov.hmrc.mongo"             %% "hmrc-mongo-play-30"         % mongoVersion,
    "com.github.java-json-tools"    %  "json-schema-validator"      % "2.2.14",
    "com.fasterxml.jackson.module"  %% "jackson-module-scala"       % "2.16.1"
  )

  val test = Seq(
    "uk.gov.hmrc"             %% "bootstrap-test-play-30"     % bootstrapVersion                 % "test, it",
    "de.flapdoodle.embed"     %  "de.flapdoodle.embed.mongo"  % "3.5.3"                 % Test,
    "uk.gov.hmrc.mongo"       %% "hmrc-mongo-test-play-30"    % mongoVersion           % Test,
    "com.vladsch.flexmark"    %  "flexmark-all"               % "0.64.0"                % "test, it",
    "org.scalatest"           %% "scalatest"                  % "3.2.15"                % Test,
    "org.scalatestplus.play"  %% "scalatestplus-play"         % "5.1.0"                 % Test,
    "org.scalatestplus"       %% "mockito-4-6"                % "3.2.15.0"              % Test,
    "org.scalatestplus"       %% "scalacheck-1-17"            % "3.2.15.0"              % Test
  )

  def apply(): Seq[ModuleID] = compile ++ test
}
