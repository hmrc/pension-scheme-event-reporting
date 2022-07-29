/*
 * Copyright 2022 HM Revenue & Customs
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

package experimental

import org.apache.spark.sql
import org.apache.spark.sql.types.{BooleanType, DateType, IntegerType, StringType, StructField, StructType}
import org.apache.spark.sql.{Dataset, Row, SparkSession}

/**
 * This is a runnable object for the purposes of experimentation while working on our desired implementation.
 */

object fileUpload extends App {

  // Path to dummy spreadsheet with three tabs.
  val path = "app/experimental/testing-spreadsheet.xlsx"

  // Creating a SparkSession.
  val spark = SparkSession.builder()
    .appName("Experimental DF")
    .config("spark.master", "local[*]")
    .getOrCreate()

  // Schema which matches the expected schema of the table in the dummy spreadsheet.
  val dummySchema = StructType(Array(
    StructField("Name", StringType),
    StructField("Role", StringType),
    StructField("Company", StringType),
    StructField("Made this spreadsheet", BooleanType),
    StructField("Date of birth", DateType),
    StructField("Preferred number of screens", IntegerType)
  ))

  // Method to read a single tab of a spreadsheet and put into a DataFrame.
  def readTabOfSpreadsheet(schema: StructType, tab: Int, path: String): sql.DataFrame = {
    spark.read
      .format("com.crealytics.spark.excel")
      .schema(schema)
      .option("header", "true")
      .option("dataAddress", s"$tab!")
      .option("useHeader", "true")
      .option("treatEmptyValuesAsNulls", "false")
      .option("location", path)
      .option("addColorColumns", "false")
      .load(path)
  }

  // Reading all tabs of dummy spreadsheet.
  val dfSheet0 = readTabOfSpreadsheet(dummySchema, 0, path)
  val dfSheet1 = readTabOfSpreadsheet(dummySchema, 1, path)
  val dfSheet2 = readTabOfSpreadsheet(dummySchema, 2, path)

  // Union of all tabs.
  val allDfs: Dataset[Row] = dfSheet0.union(dfSheet1).union(dfSheet2)

  // Removing null rows.
  val filtered: Dataset[Row] = allDfs.filter("Name is not null")

  // Show Dataset in terminal.
  filtered.show() // Unit

  /**
   * Convert Dataset into JSON documents (one per tab).
   * You'll have to delete the folder this creates if you want to run again as doesn't overwrite.
   */
  // val asJson = filtered.write.json("app/experimental/testing-spreadsheet-as-json")
}

