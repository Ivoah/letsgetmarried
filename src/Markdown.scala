package net.ivoah.letsgetmarried

import org.commonmark.ext.gfm.strikethrough.StrikethroughExtension
import org.commonmark.parser.Parser
import org.commonmark.renderer.html.HtmlRenderer
import scalatags.Text.all.*
import scala.jdk.CollectionConverters.*
import play.api.libs.json.*

case class Markdown(content: String)
given Format[Markdown] {
	def reads(v: JsValue): JsResult[Markdown] = v match {
		case JsString(str) => JsSuccess(Markdown(str))
		case _ => JsError()
	}
	def writes(s: Markdown): JsValue = JsString(s.content)
}
given FormBuilder[Markdown] = (s, n) => textarea(name:=n, s.map(_.content))
given FormParser[Markdown] = (f, n) => Markdown(f(n))
given Conversion[Markdown, Frag] = m => Markdown.render(m)

object Markdown {
  private val extensions = Seq(StrikethroughExtension.create())
  private val parser = Parser.builder().extensions(extensions.asJava).build()
  private val htmlRenderer = HtmlRenderer.builder().extensions(extensions.asJava).build()

  def render(markdown: Markdown): Frag = raw(htmlRenderer.render(parser.parse(markdown.content)))
}
