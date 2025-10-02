package net.ivoah.letsgetmarried

import com.typesafe.config.ConfigFactory

import java.sql.ResultSet

case class RegistryItem(id: Int, name: String, url: String, image: String, count: Int, price: Double)
object RegistryItem {
  val table: RawQuery = rsql"registry"
  val columns: RawQuery = rsql"registry.id as registry_id, registry.name as registry_name, registry.url as registry_url, registry.image as registry_image, registry.count as registry_count, registry.price as registry_price"
  def fromResultSet(r: ResultSet): RegistryItem = RegistryItem(
    r.getInt("registry_id"),
    r.getString("registry_name"),
    r.getString("registry_url"),
    r.getString("registry_image"),
    r.getInt("registry_count"),
    r.getDouble("registry_price")
  )
}

object Registry {
  given Database = Database(ConfigFactory.load().getString("database.connection"))

  def items: Seq[RegistryItem] = sql"""
    SELECT ${RegistryItem.columns}
    FROM ${RegistryItem.table}
  """.query(RegistryItem.fromResultSet)
}
