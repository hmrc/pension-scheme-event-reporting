package models

import play.api.libs.json.{JsObject, Json}

case class Address(addressLine1: String,
                   addressLine2: String,
                   addressLine3: Option[String],
                   addressLine4: Option[String],
                   postcode: Option[String],
                   country: String) {
  def toUA: JsObject = Json.obj(
    "addressLine1" -> addressLine1,
    "addressLine2" -> addressLine2,
    "country" -> country
  ) ++ addressLine3.fold(Json.obj()) { addr =>
    Json.obj("addressLine3" -> addr)
  } ++
    addressLine4.fold(Json.obj()) { addr =>
      Json.obj("addressLine3" -> addr)
    } ++
    postcode.fold(Json.obj()) { postcode =>
      Json.obj("postcode" -> postcode)
    }

  def toTarget: JsObject = Json.obj(
    "addressLine1" -> addressLine1,
    "addressLine2" -> addressLine2,
    "countryCode" -> country
  ) ++ addressLine3.fold(Json.obj()) { addr =>
    Json.obj("addressLine3" -> addr)
  } ++
    addressLine4.fold(Json.obj()) { addr =>
      Json.obj("addressLine3" -> addr)
    } ++
    postcode.fold(Json.obj()) { postcode =>
      Json.obj("postCode" -> postcode)
    }
}
