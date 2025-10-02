package net.ivoah.letsgetmarried

import org.commonmark.parser.Parser
import org.commonmark.renderer.html.HtmlRenderer
import scalatags.Text.all.{Frag, raw}

object Markdown {
  private val parser = Parser.builder().build()
  private val htmlRenderer = HtmlRenderer.builder().build()
  
  def render(markdown: String): Frag = raw(htmlRenderer.render(parser.parse(markdown)))
}
