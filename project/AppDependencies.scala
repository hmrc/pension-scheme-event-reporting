import sbt._

object AppDependencies {

  val compile = Seq(
    ("uk.gov.hmrc"             %% "bootstrap-backend-play-28"  % "5.24.0").exclude("", ""),
    "uk.gov.hmrc.mongo"       %% "hmrc-mongo-play-28"         % "0.64.0",
    "com.github.java-json-tools" %% "json-schema-validator" % "2.2.14",
    "org.apache.spark" %% "spark-core" % "3.2.1",
    "org.apache.spark" %% "spark-sql" % "3.2.1",
    "com.crealytics" %% "spark-excel" % "0.14.0",
    "org.apache.logging.log4j" % "log4j-api" % "2.4.1",
    "org.apache.logging.log4j" % "log4j-core" % "2.4.1"
  )

  val test = Seq(
    "uk.gov.hmrc"             %% "bootstrap-test-play-28"     % "5.24.0"              % "test, it",
    "com.github.simplyscala"  %% "scalatest-embedmongo"       % "0.2.4"               % Test,
    "uk.gov.hmrc.mongo"       %% "hmrc-mongo-test-play-28"    % "0.64.0"              % Test,
    "com.vladsch.flexmark"    %  "flexmark-all"               % "0.36.8"              % "test, it",
    "org.mockito"             % "mockito-core"                % "4.0.0"               % "test",
    "org.mockito"             %% "mockito-scala"              % "1.16.42"             % "test",
    "org.mockito"             %% "mockito-scala-scalatest"    % "1.16.42"             % "test",
    "com.github.tomakehurst"  %  "wiremock"                 % "2.26.0"                % "test",
    "com.github.tomakehurst"  %  "wiremock-jre8"            % "2.26.0"                % "test",

  )

  def apply(): Seq[ModuleID] = compile ++ test
}
