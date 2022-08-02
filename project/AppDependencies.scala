import sbt._

object AppDependencies {

  val compile = Seq(
    "uk.gov.hmrc"             %% "bootstrap-backend-play-28"  % "5.24.0",
    "uk.gov.hmrc.mongo"       %% "hmrc-mongo-play-28"         % "0.68.0",
    "com.github.java-json-tools" %% "json-schema-validator" % "2.2.14"
  )

  val test = Seq(
    "uk.gov.hmrc"             %% "bootstrap-test-play-28"     % "5.24.0"              % "test, it",
    "com.github.simplyscala"  %% "scalatest-embedmongo"       % "0.2.4"               % Test,
    "uk.gov.hmrc.mongo"       %% "hmrc-mongo-test-play-28"    % "0.68.0"              % Test,
    "com.vladsch.flexmark"    %  "flexmark-all"               % "0.36.8"              % "test, it",
    "org.mockito"             % "mockito-core"                % "4.0.0"               % "test",
    "org.mockito"             %% "mockito-scala"              % "1.16.42"             % "test",
    "org.mockito"             %% "mockito-scala-scalatest"    % "1.16.42"             % "test",
    "com.github.tomakehurst"  %  "wiremock"                 % "2.26.0"                % "test",
    "com.github.tomakehurst"  %  "wiremock-jre8"            % "2.26.0"                % "test",

  )

  def apply(): Seq[ModuleID] = compile ++ test
}
