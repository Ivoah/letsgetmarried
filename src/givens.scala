package net.ivoah.letsgetmarried

import org.virtuslab.yaml.*

import java.time.{LocalDate, LocalDateTime}

given YamlCodec[LocalDate] = new YamlCodec[LocalDate] {
	override def construct(node: Node)(implicit settings: LoadSettings): Either[ConstructError, LocalDate] = {
		node.as[String]
			.map(LocalDate.parse)
			.left.map(_.asInstanceOf[ConstructError])
	}

	override def asNode(ld: LocalDate): Node = Node.ScalarNode(ld.toString)
}

given YamlCodec[LocalDateTime] = new YamlCodec[LocalDateTime] {
	override def construct(node: Node)(implicit settings: LoadSettings): Either[ConstructError, LocalDateTime] = {
		node.as[String]
			.map(LocalDateTime.parse)
			.left.map(_.asInstanceOf[ConstructError])
	}

	override def asNode(ldt: LocalDateTime): Node = Node.ScalarNode(ldt.toString)
}
