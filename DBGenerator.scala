package models.v2

import java.io.File

import org.apache.commons.io.FileUtils

import anorm.SQL
import anorm.SqlParser.flatten
import anorm.SqlParser.str
import play.api.Play.current
import play.api.db.DB

object DBGenerator {

  def anormType(col: (String, String, String)) = {
    val t = col._2.toLowerCase match {
      case s if s.startsWith("tinyint(1)") => "Boolean"
      case s if s.contains("int") => "Int"
      case s if s.startsWith("char") => "String"
      case s if s.startsWith("text") => "String"
      case s if s.startsWith("decimal") => "java.math.BigDecimal"
      case s if s.startsWith("timestamp") => "org.joda.time.DateTime"
      case s if s.startsWith("bigint") => "Long"
      case s if s.startsWith("enum") => "String"
      case s if s.startsWith("varchar") => "String"
      case s if s.startsWith("datetime") => "org.joda.time.DateTime"
      case other => other
    }

    if (col._1.equalsIgnoreCase("id")) "Option[Long]"
    else if (col._3.equalsIgnoreCase("YES")) "Option[%s]" format (t)
    else t
  }

  def getCaseClassFieldList(tableName: String) = {
    getColumns(tableName).map { r =>
      "%s: %s" format (typeToTyp(r._1), anormType(r))
    }
  }
  
  def getColumns(tableName: String) = {
    val q = """show columns from `%s`""" format (tableName)

    DB.withConnection { implicit c =>
      SQL(q).as(str("COLUMNS.COLUMN_NAME") ~ str("COLUMNS.COLUMN_TYPE") ~ str("COLUMNS.IS_NULLABLE") map(flatten) *)
    }
  }

  def generateCaseClass(tableName: String, fields: List[String]): String = {
    "case class %s (\n  %s\n) extends Updatable[%s]" format (tableName, fields.mkString(",\n  "), tableName)
  }

  def typeToTyp(s: String) = {
    s match {
      case "type" => "typ"
      case other => other
    }  
  }
  
  def generateAnormParser(className: String, columns: List[(String, String, String)]): String = {

    val header: String = "val " + className + "Parser = \n"

    val getters = columns.map(f =>
      "  get[" + anormType(f) + "](\"" + f._1 + "\")").mkString(" ~ \n")

    val mapper = " map {\n  case " + columns.map(f => typeToTyp(f._1)).mkString(" ~ ") +
      " =>\n  " + className + "(" + columns.map(f => typeToTyp(f._1)).mkString(", ") + ")\n  }\n"

    header + getters + mapper
  }

  def getTables = {
    DB.withConnection { implicit c =>
      val q = "show tables"
      SQL(q).as(str("TABLE_NAMES.TABLE_NAME") *).filter(f => !f.equalsIgnoreCase("play_evolutions"))
    }
  }
  
  def main = {

    val tables = getTables
    
    val columns = tables.map(t => t -> getColumns(t)).toMap
    
    val h = "package models.v2\n\nimport anorm._\n\nimport anorm.SqlParser._\n\nimport utils.AnormExtention._\n\nobject Parsers {\n "
    val ps = tables.map(t => generateAnormParser(t, columns(t))).mkString("\n\n")	
    
    val psSp = DBGenerator2.tableNames.map { x =>
      val className = DBGenerator2.deriveClassName(x._1)
      val fields = DBGenerator2.getFieldList(x._1)
      DBGenerator2.genParser(className, fields)
    }.mkString("\n\n")
    
    val map = "\nval parsers = Map[String, RowParser[_]](\n\n" + (tables ++ DBGenerator2.tableNames.map(x => x._1)).map(t => "\"" + t + "\" -> " + t + "Parser").mkString(",\n")
    val t = ")\n}"
    
    FileUtils.writeStringToFile(new File("./app/models/v2/Parsers.scala"), h + ps + psSp + map + t)
    
    println(h + ps + t)
    
    val cs = tables.map(t => generateCaseClass(t, getCaseClassFieldList(t))).mkString("\n\n")
    val csSp = DBGenerator2.tableNames.map { x =>
      val className = DBGenerator2.deriveClassName(x._1)
      val fields = DBGenerator2.getFieldList(x._1)
      DBGenerator2.genClassDef(className, fields)
    }.mkString("\n\n")
    
    FileUtils.writeStringToFile(new File("./app/models/v2/Entities.scala"), "package models.v2\n\n" + cs + "\n\n" + csSp)
    
    println(cs)

    val h1 = "package models.v2\n\nimport play.api.libs.json._\nimport play.api.libs.json.Json._\nimport play.api.libs.json.Writes._\n" +
    		"import play.api.libs.functional.syntax._\nimport org.joda.time.DateTime\nobject JsonFormats {\n\n  implicit object javaBigDecimalFormat extends Format[java.math.BigDecimal] {\n" +
    		"    def reads(json: JsValue): JsResult[java.math.BigDecimal] = JsSuccess(new java.math.BigDecimal(json.as[scala.math.BigDecimal].doubleValue))\n" +
    		"    def writes(o: java.math.BigDecimal): JsValue = Json.toJson(scala.math.BigDecimal(o))\n  }\n\n"
    val ps2 = (tables ++ DBGenerator2.tableNames.map(x => x._1)).map(t => "  implicit val %sFormat = Json.format[%s]" format (t, t)).mkString("\n\n")
    val t2 = "\n}"
      
    FileUtils.writeStringToFile(new File("./app/models/v2/JsonFormats.scala"), h1 + ps2 + t2)
  }
  
  def main2 = {
    DB.withConnection { implicit c =>
      val q = "call CloudStatus({state})"
      SQL(q).on('state -> 1)().map { row =>
        row.metaData.availableColumns
       
      }.toList
    }.foreach(println)
  }
}
