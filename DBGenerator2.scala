package models.v2

import scala.sys.process._
import anorm._
import play.api.db.DB
import play.api.Play.current

/**
 * Generate a parser combinator for a specified table in the database.
 * Right now it's just specified with the val "tableName" a few lines
 * down.  
 * 
 * 20121024 bwbecker
 */
object DBGenerator2 {

  val tableNames = Map(
      "OcrUserStatus" -> "call OcrUserStatus(1)", 
      "OcrUserItemStatus" -> "Call OcrUserItemStatus(2)", 
      "OcrUserPhotoStatus" -> "call OcrUserPhotoStatus(1979, 1996)",
      "BookInfo"	-> "call BookInfo(11)",
      "ItemWithDetail" -> "call ItemWithDetail(1976)",
      "LabGroupRangeCache" -> "call LabGroupRangeCache()",
      "LabResultDetail" -> "call LabResultDetail(2121)",
      "SimpleConditionInfo" -> "call SimpleConditionInfo(1)",
      "SimpleLabInfo" -> "call SimpleLabInfo(1)",
      "SimpleDrugInfo" -> "call SimpleDrugInfo(1)",
      "MobileConditionInfo" -> "call MobileConditionInfo(3, 1)",
      "SimplePhotoInfo" -> "call SimplePhotoInfo(1)"
  )

  /** 
   * Convert the sql type to an equivalent Scala type.
   */
  def fieldType(field:MetaDataItem):String = {
    val t = field.clazz match {
      
      case "java.lang.String" => "String"
      case "java.lang.Boolean" => "Boolean"
      case "java.lang.Long" => "Long"
      case "java.lang.Integer" => "Int"
      // case "java.math.BigDecimal" => "java.math.BigDecimal"
      case "java.sql.Timestamp" => "org.joda.time.DateTime"
      case other => other
    }

    if (field.column.qualified.endsWith(".id")) "Option[Long]"
    else if (field.nullable) "Option[%s]" format (t)
    else t
  }

  /**
   * Drop the schema name from a string (tablename or fieldname)
   */
  def dropSchemaName(str:String):String = 
    str.dropWhile(c => c != '.').drop(1)

  def formatField(field:MetaDataItem):String = {
    "\t" + dropSchemaName(field.column.qualified) + " : " + fieldType(field)
  }

  /** 
   * Derive the class name from the table name:  drop the schema,
   * remove the underscores, and capitalize the leading letter of each word.
   */
  def deriveClassName(tableName:String) = 
    tableName.split("_").map(w => w.head.toUpper + w.tail).mkString

  /** 
   * Query the database to get the metadata for the given table.
   */
  def getFieldList(tableName:String):List[MetaDataItem] = {
      val sql = SQL(tableNames(tableName))

      val results:Stream[SqlRow] = DB.withConnection { implicit connection => sql()  }
      
      results.head.metaData.ms
    }

  /**
   * Generate a case class definition with one data member for each field in
   * the database table.
   */
  def genClassDef(className:String, fields:List[MetaDataItem]):String = {
    val fieldList = fields.map(formatField(_)).mkString(",\n")

    """    case class %s (
    %s
    ) extends Updatable[%s]
    """ format (className, fieldList, className )
  }

  /**
   * Generate a parser for the table. 
   */
  def genParser(className:String, fields:List[MetaDataItem]):String = {

    val header:String = "val " + className + 
    "Parser:RowParser[" + className + "] = {\n"

    val getters = fields.map(f => 
      "\tget[" + fieldType(f) + "](\"" + dropSchemaName(f.column.qualified) + "\")"
    ).mkString(" ~ \n") 

    val mapper = " map {\n      case " + fields.map(f => dropSchemaName(f.column.qualified)).mkString(" ~ ") +
        " =>\n\t" + className + "(" + fields.map(f => dropSchemaName(f.column.qualified)).mkString(", ") + ")\n\t}\n}"

    header + getters + mapper
  }

  def main() = {

    tableNames.foreach { x =>
      val className = deriveClassName(x._1)
      val fields = getFieldList(x._1)

      println(genClassDef(className, fields))

      println(genParser(className, fields))
    }
  }
}