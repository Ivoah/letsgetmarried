package net.ivoah.letsgetmarried

import scala.util.Using
import java.sql.*
import java.time.LocalDate

type QueryParam = Int | Double | String | Boolean | LocalDate

case class Database(url: String) {
  var db: Connection = DriverManager.getConnection(url)

  def connection: Connection = {
    if (!db.isValid(5)) db = DriverManager.getConnection(url)
    db
  }
}

case class RawQuery(query: String) {
  override def toString: String = query
  def +(other: RawQuery): RawQuery = RawQuery(query + other.query)
  def +(other: String): RawQuery = RawQuery(query + other)
}

case class Query(parts: Seq[String], params: Seq[QueryParam | RawQuery]) {
  private def buildStatement(using db: Database): PreparedStatement = {
    val stmt = db.connection.prepareStatement(
      parts.zipAll(params, "", rsql"").map {
        case (part, _: QueryParam) => s"$part?"
        case (part, param: RawQuery) => s"$part${param.query}"
      }.mkString,
      Statement.RETURN_GENERATED_KEYS
    )

    for ((param, i) <- params.collect { case p: QueryParam => p }.zipWithIndex) param match {
      case int: Int        => stmt.setInt(i + 1, int)
      case dbl: Double     => stmt.setDouble(i + 1, dbl)
      case str: String     => stmt.setString(i + 1, str)
      case bool: Boolean   => stmt.setBoolean(i + 1, bool)
      case date: LocalDate => stmt.setString(i + 1, date.toString)
    }

    stmt
  }

  def query[T](using db: Database)(fn: ResultSet => T): Seq[T] = {
    Using.resource(buildStatement) { stmt =>
      Iterator.unfold(stmt.executeQuery()) { results =>
        if (results.next()) Some(fn(results), results)
        else None
      }.toSeq
    }
  }

  def update(using db: Database)(): Int = Using.resource(buildStatement)(_.executeUpdate())

  def updateGetKey(using db: Database)(): Int = Using.resource(buildStatement) { stmt =>
    stmt.executeUpdate()
    val results = stmt.getGeneratedKeys
    results.next()
    results.getInt(1)
  }

  def +(other: String): Query = Query(parts.init :+ (parts.last + other), params)
  def +(other: RawQuery): Query = Query(parts.init :+ (parts.last + other.query), params)
  def +(other: Query): Query = Query(parts.init ++ Seq(parts.last + other.parts.head) ++ other.parts.tail, params ++ other.params)
}

extension (sc: StringContext) {
  def sql(params: (QueryParam | RawQuery)*): Query = Query(sc.parts, params)
  def rsql(args: Any*): RawQuery = RawQuery(sc.s(args*))
}
