package net.ivoah.letsgetmarried

import org.commonmark.ext.gfm.strikethrough.StrikethroughExtension
import org.commonmark.parser.Parser
import org.commonmark.renderer.html.HtmlRenderer
import scalatags.Text.all.*
import scala.jdk.CollectionConverters.*
import play.api.libs.json.*

case class Markdown(content: String)
given StringWrapper[Markdown] {
	def init(s: String) = Markdown(s)
	def content(s: Markdown): String = s.content
	def tag(m: Option[Markdown]): Tag = textarea(m.map(_.content))
}
given Conversion[Markdown, Frag] = m => Markdown.render(m)

object Markdown {
  private val extensions = Seq(StrikethroughExtension.create())
  private val parser = Parser.builder().extensions(extensions.asJava).build()
  private val htmlRenderer = HtmlRenderer.builder().extensions(extensions.asJava).build()

  def render(markdown: Markdown): Frag = raw(htmlRenderer.render(parser.parse(markdown.content)))
}
