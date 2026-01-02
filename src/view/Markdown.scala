package net.ivoah.letsgetmarried
package view

import org.commonmark.ext.gfm.strikethrough.StrikethroughExtension
import org.commonmark.parser.Parser
import org.commonmark.renderer.html.HtmlRenderer
import scalatags.Text.all.{Frag, raw, StringFrag}
import scala.jdk.CollectionConverters.*

object Markdown {
  private val extensions = Seq(StrikethroughExtension.create())
  private val parser = Parser.builder().extensions(extensions.asJava).build()
  private val htmlRenderer = HtmlRenderer.builder().extensions(extensions.asJava).build()

  def render(markdown: StringFrag): Frag = raw(htmlRenderer.render(parser.parse(markdown.render)))
}
