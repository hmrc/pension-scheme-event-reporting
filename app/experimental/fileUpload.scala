
package experimental

import org.apache.spark.sql.types.{BooleanType, DateType, IntegerType, StringType, StructField, StructType}
import org.apache.spark.sql.SparkSession

object fileUpload extends App {

  val path = "app/experimental/testing-spreadsheet.xlsx"

  // creating a SparkSession
  val spark = SparkSession.builder()
    .appName("NJ's DF")
    .config("spark.master", "local[*]")
    .getOrCreate()

  // schema
  val njSchema = StructType(Array(
    StructField("Name", StringType),
    StructField("Role", StringType),
    StructField("Company", StringType),
    StructField("Made this spreadsheet", BooleanType),
    StructField("Date of birth", DateType),
    StructField("Preferred number of screens", IntegerType)
  ))


  // reading a DF
  val dfSheet0 = spark.read
    .format("com.crealytics.spark.excel")
    .schema(njSchema)
    .option("header", "true")
    .option("dataAddress", "0!")
    .option("useHeader", "true")
    .option("treatEmptyValuesAsNulls", "false")
    .option("location", path)
    .option("addColorColumns", "false")
    .load(path)

  val dfSheet1 = spark.read
    .format("com.crealytics.spark.excel")
    .schema(njSchema)
    .option("header", "true")
    .option("dataAddress", "1!")
    .option("useHeader", "true")
    .option("treatEmptyValuesAsNulls", "false")
    .option("location", path)
    .option("addColorColumns", "false")
    .load(path)

  val dfSheet2 = spark.read
    .format("com.crealytics.spark.excel")
    .schema(njSchema)
    .option("header", "true")
    .option("dataAddress", "2!")
    .option("useHeader", "true")
    .option("treatEmptyValuesAsNulls", "false")
    .option("location", path)
    .option("addColorColumns", "false")
    .load(path)

  val allDfs = dfSheet0.union(dfSheet1).union(dfSheet2)
  val filtered = allDfs.filter("Name is not null")

  // Show DF in terminal
  filtered.show()
}
  // Convert DF into JSON
  //  val asJson = filtered.write.json("src/main/resources/data/my-example")
