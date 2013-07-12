package models.v2

import org.joda.time.DateTime
import anorm._
import utils.AnormExtention._
import anorm.SqlParser._
import play.api.Play.current
import play.api.db.DB
import java.io.File
import ch.qos.logback.core.util.FileUtil
import org.apache.commons.io.FileUtils
import play.Logger

/*
  
  import java.io.File
  import play.core._
  new StaticApplication(new File("."))
  import models.v2._
  import helpers.v2._
  
 */

object Updatable {
  
  def find[A](sql: String)(implicit ma: Manifest[A]) = {
    val a = ma.runtimeClass.getSimpleName
    
    DB.withConnection { implicit conn =>
      SQL(sql).as(Parsers.parsers(a) *).asInstanceOf[List[A]]
    }
  }
  
  def valuesToParameters(values: (String, Any)*) = {
    values.map { x =>
      (x._1, x._1 match {
        case y if y.contains("Date") => {
          dateTimeToParameterValue(x._2.asInstanceOf[Option[org.joda.time.DateTime]].get)
        }
        case _ =>
          x._2 match {
            case y: DateTime => dateTimeToParameterValue(y)
            case z => toParameterValue(z)
          }
      })
    }
  }
  
  def dateTimeToParameterValue(a: DateTime)(implicit p: ToStatement[String]): ParameterValue[String] = ParameterValue(a.toString("yyyy-MM-dd hh:mm:ss"), p)

  def toParameterValue[A](a: A)(implicit p: ToStatement[A]): ParameterValue[A] = ParameterValue(a, p)
  
  def sql(sql: String) = {
    DB.withConnection { implicit conn =>
      SQL(sql).executeUpdate
    }
  }
  
  def delete(tableName: String, values: (String, Any) *) = {
    DB.withConnection { implicit c =>

      val on = valuesToParameters(values:_*)

      val conditions = values.map(v => v._1 + "={" + v._1 + "}").mkString(" and ")
      
      val q = "delete from `" + tableName + "` where " + conditions
      SQL(q).on(on: _*).executeUpdate

    }
  }

  
  def update(id: (String, Any), tableName: String, values: (String, Any) *) = {
    DB.withConnection { implicit c =>

      val on = valuesToParameters((values.toList.::(id)):_*)

      val u = values.map(x => x._1 + "={" + x._1 + "}").mkString(",")
        val q = "update `" + tableName + "` set " + u + " where " + id._1 + "={" + id._1 + "}"
        SQL(q).on(on: _*).executeUpdate
    }
  }
  
  def save(id: Option[Long], tableName: String, values: (String, Any)*) = {
    
    DB.withConnection { implicit c =>

      val on = valuesToParameters(values:_*)

      id.fold {

        val i = values.map(x => x._1).mkString(",") + ", created"
        val u = values.map(x => "{" + x._1 + "}").mkString(",") + ", {created}"
        val q = "insert into `" + tableName + "`(" + i + ") values(" + u + ")"

        val created = "created" -> dateTimeToParameterValue(DateTime.now)

        SQL(q).on((on :+ created): _*).executeInsert().get
      } { real =>

        val u = values.map(x => x._1 + "={" + x._1 + "}").mkString(",")
        val q = "update `" + tableName + "` set " + u + " where id=" + id.get
        
        Logger.debug(q)
        
        SQL(q).on(on: _*).executeUpdate
        
        real
      }
    }
  }

  def load[A](id: Long)(implicit e: Manifest[A]) = {
    DB.withConnection { implicit c =>
      val className = e.runtimeClass.getSimpleName
      val q = "select * from `" + className + "` where id=" + id
      
      SQL(q).as(Parsers.parsers(className).singleOpt).asInstanceOf[Option[A]]
      // SQL(q).as(Parsers.parsers(className).singleOpt).asInstanceOf[Option[A]]
    }
  }
  
  def find[A](values: (String, Any)*)(implicit e: Manifest[A]): List[A] = {
    val conditions = values.map(v => v._1 + "={" + v._1 + "}").mkString(" and ")
      
    find(conditions, values: _*)
  }
  
  def findWithOperators[A](values: (String, String, Any) *)(implicit e: Manifest[A]): List[A] = {
    val conditions = values.map(v => v._1 + v._2 + "{" + v._1 + "}").mkString(" and ")
    val values2 = values.map(v => (v._1, v._3))
    
    find(conditions, values2: _*)
  }
  
  def find[A](conditions: String, values: (String, Any) *)(implicit e: Manifest[A]): List[A] = {
    DB.withConnection { implicit c =>
      val className = e.runtimeClass.getSimpleName

      val on = valuesToParameters(values:_*)

      val cs = if (conditions.size > 0) {
        "` where " + conditions
      } else {
        "`"
      }
      
      val q = "select * from `" + className + cs + " order by created desc"
      
      SQL(q).on(on: _*).as(Parsers.parsers(className) *).asInstanceOf[List[A]]
      // SQL(q).as(Parsers.parsers(className).singleOpt).asInstanceOf[Option[A]]
    }
  }
  
  def singleLink[A, B](aId: Long, a2B: Any)(implicit ea: Manifest[A], eb: Manifest[B]) = {
    DB.withConnection { implicit c =>
      
      val ta = ea.runtimeClass.getSimpleName
      val tb = eb.runtimeClass.getSimpleName
      
      val q = "select tb.* from `" + ta + "` ta left join `" + tb + "` tb on ta." + a2B + "=tb.id where ta.id={aId} limit 1"

      SQL(q).on('aId -> aId).as(Parsers.parsers(tb).singleOpt).asInstanceOf[Option[B]]
    }
  }
  
  def call[A](parameters: Any *)(implicit e: Manifest[A]) = {
    DB.withConnection { implicit c =>
      
      val spName = e.runtimeClass.getSimpleName
      
      val q = "call " + spName + "(" + (0 to parameters.length-1).map(x => "{p" + x + "}").mkString(",") + ")"
      
      val on = valuesToParameters(parameters.zipWithIndex.map(x => ("p" + x._2, x._1)):_*)
      
      SQL(q).on(on: _*).as(Parsers.parsers(spName) *).asInstanceOf[List[A]]
    }
  }
}

trait Updatable[A] { self: A =>

  val id: Option[Long]

  def save(values: (String, Any)*): Unit = {

    Updatable.save(id, getClass.getSimpleName, values: _*)
  }

  def save(): Unit = {

    val fs = self.getClass.getDeclaredFields
    val values = fs.filter(f => !f.getName.equalsIgnoreCase("id") && !f.getName.equalsIgnoreCase("created") && !f.getName.equalsIgnoreCase("updated")).map { f =>
      f.setAccessible(true)
      (f.getName, f.get(self))
    }
    
    save(values: _*)
  }

  def reload(implicit e: Manifest[A]) = {
    Updatable.load[A](id.get)
  }
  
  def singleLink[B](a2B: Any)(implicit ea: Manifest[A], eb: Manifest[B]) = {
    Updatable.singleLink[A, B](id.get, a2B)
  }
}



