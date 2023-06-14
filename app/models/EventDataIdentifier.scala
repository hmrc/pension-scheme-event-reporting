package models

import models.enumeration.ApiType

case class EventDataIdentifier(apiType: ApiType, year: Int, version: Int)
