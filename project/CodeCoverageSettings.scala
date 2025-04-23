import sbt.Setting
import scoverage.ScoverageKeys

object CodeCoverageSettings {

  private val excludedPackages: Seq[String] = Seq(
    "<empty>",
    "Reverse.*",
    "uk.gov.hmrc.BuildInfo",
    "app.*",
    "prod.*",
    ".*Routes.*",
    "testOnly.*",
    "testOnlyDoNotUseInAppConf.*",
    "target.*"
  )

  private val coverageExcludedFiles = "<empty>;Reverse.*;.*handlers.*;.*repositories.*;" +
    ".*BuildInfo.*;.*Routes.*;.*javascript.*;.*GuiceInjector;.*AppConfig;.*Module;" +
    ".*ControllerConfiguration;.*TestController;.*LanguageSwitchController;" +
    ".*CompileEventAuditEvent.*;.*SubmitEventDeclarationAuditEvent.*;.*EncryptedValue.*;.*ReportVersion.*;.*ApiType.*"

  val settings: Seq[Setting[_]] = Seq(
    ScoverageKeys.coverageExcludedFiles := coverageExcludedFiles,
    ScoverageKeys.coverageExcludedPackages := excludedPackages.mkString(";"),
    ScoverageKeys.coverageMinimumStmtTotal := 80,
    ScoverageKeys.coverageFailOnMinimum := true,
    ScoverageKeys.coverageHighlighting := true
  )
}
