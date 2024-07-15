/*
 * Copyright 2024 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package transformations.ETMPToFrontEnd

import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import transformations.Transformer

object API1833 {

  import API1833Paths._
  import API1833ReadsUtilities._

  implicit val rds1833Api: Reads[JsObject] =
    pathEtmpEvent1Details.readNullable(readsEvent1Details).flatMap {
      case Some(jsArray) => pathUAEvent1MembersOrEmployers.json.put(jsArray)
      case _ => Reads.pure(Json.obj())
    }

}

private object API1833ReadsUtilities extends Transformer {

  import API1833Paths._

  lazy val readsEvent1Details: Reads[JsArray] = __.read(
    Reads.seq(
      (
        readsMemberType and
          readsIndividualOrEmployerMemberDetails and
          readsUnAuthorisedPaymentDetails and
          readsMemberChangeInfo
        ).reduce
    ).map(JsArray(_)))

  private val readsMemberType: Reads[JsObject] = reqReadsStrTransform(pathUAWhoReceivedUnauthPayment, pathEtmpMemberType, memberTypeTransform)

  private val readsIndividualOrEmployerMemberDetails: Reads[JsObject] = pathEtmpMemberType.json.pick.flatMap {
    case JsString("Individual") => readsIndividualMemberDetails
    case JsString("Employer") => readsEmployerMemberDetails
    case _ => fail[JsObject]
  }

  private val readsIndividualMemberDetails: Reads[JsObject] = (
    reqReads(pathUAMembersDetailsFirstName, pathEtmpIndividualMemberDetailsFirstName) and
      reqReads(pathUAMembersDetailsLastName, pathEtmpIndividualMemberDetailsLastName) and
      reqReads(pathUAMembersDetailsNino, pathEtmpIndividualMemberDetailsNino) and
      optReadsBoolTransform(pathUADoYouHoldSignedMandate, pathEtmpIndividualMemberDetailsSignedMandate, yesNoTransformToBoolean) and
      optReadsBoolTransform(pathUAValueOfUnauthorisedPayment, pathEtmpIndividualMemberDetailsPmtMoreThan25PerFundValue, yesNoTransformToBoolean) and
      optReadsBoolTransform(pathUASchemeUnAuthPaySurchargeMember, pathEtmpIndividualMemberDetailsSchemePayingSurcharge, yesNoTransformToBoolean)
    ).reduce

  private val readsEmployerMemberDetails: Reads[JsObject] = (
    reqReads(pathUACompanyName, pathEtmpEmployerMemberDetailsCompOrOrgName) and
      reqReads(pathUACompanyNumber, pathEtmpEmployerMemberDetailsCrnNumber) and
      pathUAEmployerAddress.json.copyFrom(readsAddressEtmp(pathEtmpEmployerMemberDetailsAddressDetails))
    ).reduce

  private val readsUnAuthorisedPaymentDetails: Reads[JsObject] = (
    readsUnAuthorisedPmtType1WithDynamicUAPaths and
      reqReads(pathUAUnAuthorisedPaymentDate, pathEtmpUnAuthorisedPaymentDetailsDateOfUnauthorisedPayment) and
      reqReads(pathUAUnAuthorisedPaymentValue, pathEtmpUnAuthorisedPaymentDetailsValueOfUnauthorisedPayment) and
      optReadsDynamicPathStrTransform(dynamicPathUnAuthorisedPmtType2, pathEtmpUnAuthorisedPaymentDetailsUnAuthorisedPmtType2, unAuthorisedPmtType2Transform) and
      readsFreeTxtOrSchemeOrRecipientNameWithDynamicUAPaths and
      optReads(pathUAUnAuthorisedPaymentDetailsSchemeDetailsReference, pathEtmpUnAuthorisedPaymentDetailsPstrOrReference) and
      optReads(pathUAUnAuthorisedLoanAmount, pathEtmpUnAuthorisedPaymentDetailsPmtAmtOrLoanAmt) and
      optReads(pathUAUnAuthorisedFundValue, pathEtmpUnAuthorisedPaymentDetailsFundValue) and
      readsResPropDetailsWithDynamicUAPaths
    ).reduce

  private val readsMemberChangeInfo: Reads[JsObject] = (
    reqReads(pathUAMemberStatus, pathEtmpMemberStatus) and
      optReads(pathUAAmendedVersion, pathEtmpAmendedVersion)
    ).reduce

  /**
   * These are utility functions for reading json from a given etmpPath into a given uaPath.
   * They may also include transformations of the etmp json before storing in the uaPath or doNothing if the node is missing in the response (i.e., it's an optional node).
   */

  lazy val reqReads: (JsPath, JsPath) => Reads[JsObject] = (uaPath: JsPath, etmpPath: JsPath) => uaPath.json.copyFrom(etmpPath.json.pick)

  lazy val optReads: (JsPath, JsPath) => Reads[JsObject] = (uaPath: JsPath, etmpPath: JsPath) => uaPath.json.copyFrom(etmpPath.json.pick).orElse(doNothing)

  private lazy val reqReadsStrTransform: (JsPath, JsPath, String => String) => Reads[JsObject] =
    (uaPath: JsPath, etmpPath: JsPath, transform: String => String) => {
      uaPath.json.copyFrom(etmpPath.json.pick.flatMap {
        case JsString(str) => Reads.pure(JsString(transform(str)))
        case _ => fail[JsString]
      })
    }

  private lazy val optReadsBoolTransform: (JsPath, JsPath, String => Boolean) => Reads[JsObject] =
    (uaPath: JsPath, etmpPath: JsPath, transform: String => Boolean) => {
      uaPath.json.copyFrom(etmpPath.json.pick.flatMap {
        case JsString(str) => Reads.pure(JsBoolean(transform(str)))
        case _ => fail[JsBoolean]
      }).orElse(doNothing)
    }

  private lazy val optReadsDynamicPathStrTransform: (String => JsPath, JsPath, String => String) => Reads[JsObject] =
    (dynamicUaPath: String => JsPath, etmpPath: JsPath, transform: String => String) => {
      etmpPath.json.pick.flatMap {
        case JsString(str) => dynamicUaPath(transform(str)).json.copyFrom(Reads.pure(JsString(transform(str))))
        case _ => fail[JsObject]
      }.orElse(doNothing)
    }

  /**
   * These are dynamic path functions which are required for some fields as the appropriate uaPath is different for Individual and Employer.
   */

  private val dynamicPathFreeTxtIndividual: String => JsPath = {
    case "Benefit in kind" => pathUABenefitInKindBriefDescription
    case "Transfer to non-registered pensions scheme" => pathUASchemeDetailsSchemeName
    case "Error in calculating tax free lump sums" => pathUAErrorDescription
    case "Benefits paid early other than on the grounds of ill-health, protected pension age or a winding up lump sum" => pathUABenefitsPaidEarly
    case "Tangible moveable property held directly or indirectly by an investment-regulated pension scheme" => pathUAMemberTangibleMoveableProperty
    case "Court Order Payment/Confiscation Order" => pathUAUnauthorisedPaymentRecipientName
    case "Other" => pathUAMemberPaymentNatureDescription
  }

  private val dynamicPathFreeTxtEmployer: String => JsPath = {
    case "Tangible moveable property held directly or indirectly by an investment-regulated pension scheme" => pathUAEmployerTangibleMoveableProperty
    case "Court Order Payment/Confiscation Order" => pathUAUnauthorisedPaymentRecipientName
    case "Other" => pathUAPaymentNatureDesc
    case _ => pathUAPaymentNatureEmployer
  }

  private lazy val dynamicPathUnAuthorisedPmtType2: String => JsPath = {
    case "anEmployerFinanced" | "nonRecognisedScheme" => pathUAWhoWasTheTransferMade
    case "widowOrOrphan" => pathUARefundOfContributions
    case "deathOfMember" | "deathOfDependent"
         | "dependentNoLongerQualifiedForPension"
         | "other" => pathUAReasonForTheOverpaymentOrWriteOff
  }

  /**
   * These are the most complex reads in the file because they require significant changes before being stored in a given uaPath.
   */

  private lazy val readsUnAuthorisedPmtType1IndividualOrEmployer: Reads[Option[Reads[JsObject]]] = pathEtmpMemberType.readNullable[JsString].map {
    case Some(JsString("Individual")) =>
      Some(reqReadsStrTransform(pathUAPaymentNatureMember, pathEtmpUnAuthorisedPaymentDetailsUnAuthorisedPmtType1, unAuthorisedPmtType1IndividualTransform))
    case Some(JsString("Employer")) =>
      Some(reqReadsStrTransform(pathUAPaymentNatureEmployer, pathEtmpUnAuthorisedPaymentDetailsUnAuthorisedPmtType1, unAuthorisedPmtType1EmployerTransform))
    case _ => None
  }

  private lazy val readsUnAuthorisedPmtType1WithDynamicUAPaths: Reads[JsObject] = {
    readsUnAuthorisedPmtType1IndividualOrEmployer.flatMap(_.getOrElse(Reads.pure(Json.obj())))
  }

  private lazy val readsFreeTxtOrSchemeOrRecipientName: Reads[Option[Reads[Reads[JsObject]]]] = pathEtmpMemberType.readNullable[JsString].map { optJson =>
    optJson.map {
      case JsString("Individual") =>
        pathEtmpUnAuthorisedPaymentDetailsUnAuthorisedPmtType1.readNullable[JsString].map {
          case Some(paymentNature) =>
            dynamicPathFreeTxtIndividual(paymentNature.value)
              .json.copyFrom(pathEtmpUnAuthorisedPaymentDetailsFreeTxtOrSchemeOrRecipientName.json.pick).orElse(doNothing)
          case _ => Reads.failed("Unknown EventOneReport behaviour")
        }
      case JsString("Employer") =>
        pathEtmpUnAuthorisedPaymentDetailsUnAuthorisedPmtType1.readNullable[JsString].map {
          case Some(paymentNature) => dynamicPathFreeTxtEmployer(paymentNature.value)
            .json.copyFrom(pathEtmpUnAuthorisedPaymentDetailsFreeTxtOrSchemeOrRecipientName.json.pick).orElse(doNothing)
          case _ => Reads.failed("Unknown EventOneReport behaviour")
        }
      case _ => Reads.failed("Unknown EventOneReport behaviour")
    }
  }

  private lazy val readsFreeTxtOrSchemeOrRecipientNameWithDynamicUAPaths: Reads[JsObject] = {
    readsFreeTxtOrSchemeOrRecipientName.flatMap {
      case Some(reads) => reads.flatMap(identity)
      case None => Reads.pure(Json.obj())
    }
  }

  private lazy val readsResPropDetails: Reads[Option[Reads[JsObject]]] = pathEtmpMemberType.readNullable[JsString].map {
    case Some(JsString("Individual")) => Some(optReads(pathUAMemberResidentialAddress, pathEtmpUnAuthorisedPaymentDetailsResidentialPropertyAddress))
    case Some(JsString("Employer")) => Some(optReads(pathUAEmployerResidentialAddress, pathEtmpUnAuthorisedPaymentDetailsResidentialPropertyAddress))
    case _ => None
  }

  lazy val readsResPropDetailsWithDynamicUAPaths: Reads[JsObject] = {
    readsResPropDetails.flatMap(_.getOrElse(Reads.pure(Json.obj())))
  }
}

private object API1833Paths {

  /* UserAnswers */
  val pathUAAmendedVersion:                                   JsPath = __ \ "amendedVersion"
  val pathUABenefitInKindBriefDescription:                    JsPath = __ \ "benefitInKindBriefDescription"
  val pathUABenefitsPaidEarly:                                JsPath = __ \ "benefitsPaidEarly"

  private val pathUACompanyDetails:                           JsPath = __ \ "event1" \ "companyDetails"
  val pathUACompanyName:                                      JsPath = pathUACompanyDetails \ "companyName"
  val pathUACompanyNumber:                                    JsPath = pathUACompanyDetails \ "companyNumber"

  val pathUADoYouHoldSignedMandate:                           JsPath = __ \ "doYouHoldSignedMandate"
  val pathUAEmployerAddress:                                  JsPath = __ \ "employerAddress" \ "address"
  val pathUAEmployerResidentialAddress:                       JsPath = __ \ "employerResidentialAddress"
  val pathUAEmployerTangibleMoveableProperty:                 JsPath = __ \ "employerTangibleMoveableProperty"
  val pathUAErrorDescription:                                 JsPath = __ \ "errorDescription"
  val pathUAEvent1MembersOrEmployers:                         JsPath = __ \ "event1" \ "membersOrEmployers"

  private val pathUAMemberDetails:                            JsPath = __ \ "membersDetails"
  val pathUAMembersDetailsFirstName:                          JsPath = pathUAMemberDetails \ "firstName"
  val pathUAMembersDetailsLastName:                           JsPath = pathUAMemberDetails \ "lastName"
  val pathUAMembersDetailsNino:                               JsPath = pathUAMemberDetails \ "nino"
  val pathUAMemberPaymentNatureDescription:                   JsPath = __ \ "memberPaymentNatureDescription"
  val pathUAMemberResidentialAddress:                         JsPath = __ \ "memberResidentialAddress"
  val pathUAMemberStatus:                                     JsPath = __ \ "memberStatus"
  val pathUAMemberTangibleMoveableProperty:                   JsPath = __ \ "memberTangibleMoveableProperty"

  private val pathUALoadDetails:                              JsPath = __ \ "loanDetails"
  val pathUAUnAuthorisedFundValue:                            JsPath = pathUALoadDetails \ "fundValue"
  val pathUAUnAuthorisedLoanAmount:                           JsPath = pathUALoadDetails \ "loanAmount"

  val pathUAPaymentNatureDesc:                                JsPath = __ \ "paymentNatureDesc"
  val pathUAPaymentNatureEmployer:                            JsPath = __ \ "paymentNatureEmployer"
  val pathUAPaymentNatureMember:                              JsPath = __ \ "paymentNatureMember"

  private val pathUAPaymentValueAndDate:                      JsPath = __ \ "paymentValueAndDate"
  val pathUAUnAuthorisedPaymentDate:                          JsPath = pathUAPaymentValueAndDate \ "paymentDate"
  val pathUAUnAuthorisedPaymentValue:                         JsPath = pathUAPaymentValueAndDate \ "paymentValue"

  val pathUAReasonForTheOverpaymentOrWriteOff:                JsPath = __ \ "reasonForTheOverpaymentOrWriteOff"
  val pathUARefundOfContributions:                            JsPath = __ \ "refundOfContributions"

  private val pathUASchemeDetails:                            JsPath = __ \ "schemeDetails"
  val pathUASchemeDetailsSchemeName:                          JsPath = pathUASchemeDetails \ "schemeName"
  val pathUAUnAuthorisedPaymentDetailsSchemeDetailsReference: JsPath = pathUASchemeDetails \ "reference"

  val pathUASchemeUnAuthPaySurchargeMember:                   JsPath = __ \ "schemeUnAuthPaySurchargeMember"
  val pathUAUnauthorisedPaymentRecipientName:                 JsPath = __ \ "unauthorisedPaymentRecipientName"
  val pathUAValueOfUnauthorisedPayment:                       JsPath = __ \ "valueOfUnauthorisedPayment"
  val pathUAWhoReceivedUnauthPayment:                         JsPath = __ \ "whoReceivedUnauthPayment"
  val pathUAWhoWasTheTransferMade:                            JsPath = __ \ "whoWasTheTransferMade"

  /* ETMP */
  val pathEtmpAmendedVersion:                                           JsPath = __ \ "amendedVersion"

  private val pathEtmpEmployerMemberDetails:                            JsPath = __ \ "employerMemberDetails"
  val pathEtmpEmployerMemberDetailsCompOrOrgName:                       JsPath = pathEtmpEmployerMemberDetails \ "compOrOrgName"
  val pathEtmpEmployerMemberDetailsCrnNumber:                           JsPath = pathEtmpEmployerMemberDetails \ "crnNumber"
  val pathEtmpEmployerMemberDetailsAddressDetails:                      JsPath = pathEtmpEmployerMemberDetails \ "addressDetails"

  val pathEtmpEvent1Details:                                            JsPath = __ \ "event1Details"

  private val pathEtmpIndividualMemberDetails:                          JsPath = __ \ "individualMemberDetails"
  val pathEtmpIndividualMemberDetailsFirstName:                         JsPath = pathEtmpIndividualMemberDetails \ "firstName"
  val pathEtmpIndividualMemberDetailsLastName:                          JsPath = pathEtmpIndividualMemberDetails \ "lastName"
  val pathEtmpIndividualMemberDetailsNino:                              JsPath = pathEtmpIndividualMemberDetails \ "nino"
  val pathEtmpIndividualMemberDetailsSignedMandate:                     JsPath = pathEtmpIndividualMemberDetails \ "signedMandate"
  val pathEtmpIndividualMemberDetailsPmtMoreThan25PerFundValue:         JsPath = pathEtmpIndividualMemberDetails \ "pmtMoreThan25PerFundValue"
  val pathEtmpIndividualMemberDetailsSchemePayingSurcharge:             JsPath = pathEtmpIndividualMemberDetails \ "schemePayingSurcharge"
  val pathEtmpMemberStatus:                                             JsPath = __ \ "memberStatus"
  val pathEtmpMemberType:                                               JsPath = __ \ "memberType"

  private val pathEtmpUnAuthorisedPaymentDetails:                       JsPath = __ \ "unAuthorisedPaymentDetails"
  val pathEtmpUnAuthorisedPaymentDetailsUnAuthorisedPmtType1:           JsPath = pathEtmpUnAuthorisedPaymentDetails \ "unAuthorisedPmtType1"
  val pathEtmpUnAuthorisedPaymentDetailsUnAuthorisedPmtType2:           JsPath = pathEtmpUnAuthorisedPaymentDetails \ "unAuthorisedPmtType2"
  val pathEtmpUnAuthorisedPaymentDetailsFreeTxtOrSchemeOrRecipientName: JsPath = pathEtmpUnAuthorisedPaymentDetails \ "freeTxtOrSchemeOrRecipientName"
  val pathEtmpUnAuthorisedPaymentDetailsPstrOrReference:                JsPath = pathEtmpUnAuthorisedPaymentDetails \ "pstrOrReference"
  val pathEtmpUnAuthorisedPaymentDetailsDateOfUnauthorisedPayment:      JsPath = pathEtmpUnAuthorisedPaymentDetails \ "dateOfUnauthorisedPayment"
  val pathEtmpUnAuthorisedPaymentDetailsValueOfUnauthorisedPayment:     JsPath = pathEtmpUnAuthorisedPaymentDetails \ "valueOfUnauthorisedPayment"
  val pathEtmpUnAuthorisedPaymentDetailsPmtAmtOrLoanAmt:                JsPath = pathEtmpUnAuthorisedPaymentDetails \ "pmtAmtOrLoanAmt"
  val pathEtmpUnAuthorisedPaymentDetailsFundValue:                      JsPath = pathEtmpUnAuthorisedPaymentDetails \ "fundValue"
  val pathEtmpUnAuthorisedPaymentDetailsResidentialPropertyAddress:     JsPath = pathEtmpUnAuthorisedPaymentDetails \ "residentialPropertyAddress"
}
