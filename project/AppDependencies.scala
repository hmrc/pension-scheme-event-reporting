import sbt._

object AppDependencies {

  val compile = Seq(
    "uk.gov.hmrc"             %% "bootstrap-backend-play-28"  % "5.24.0",
    "uk.gov.hmrc.mongo"       %% "hmrc-mongo-play-28"         % "0.64.0"
  )

  val test = Seq(
    "uk.gov.hmrc"             %% "bootstrap-test-play-28"     % "5.24.0"              % "test, it",
    "uk.gov.hmrc.mongo"       %% "hmrc-mongo-test-play-28"    % "0.64.0"              % Test,
    "com.vladsch.flexmark"    %  "flexmark-all"               % "0.36.8"              % "test, it",
    "org.mockito"             % "mockito-core"                % "4.0.0"               % "test",
    "org.mockito"             %% "mockito-scala"              % "1.16.42"             % "test",
    "org.mockito"             %% "mockito-scala-scalatest"    % "1.16.42"             % "test"
  )

  def apply(): Seq[ModuleID] = compile ++ test
}
