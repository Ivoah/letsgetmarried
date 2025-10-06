package net.ivoah.letsgetmarried

import java.time.LocalDate
import java.time.temporal.ChronoUnit
import scalatags.Text.all.*
import scalatags.Text.tags2.title

import java.time.format.DateTimeFormatter

object Templates {
  private val tabs = Seq(
    "Home" -> "/",
    "Our Story" -> "/story",
    "Wedding Party" -> "/party",
    "Registry" -> "/registry",
    "RSVP" -> "/rsvp"
  )

  private val fullformat = DateTimeFormatter.ofPattern("EEEE, MMMM d, yyyy")
  private val shortformat = DateTimeFormatter.ofPattern("M.d.y")

  private def _head(_title: String) = head(
    title(s"${Details.groom.split(" ").head} & ${Details.bride.split(" ").head} - $_title"),
    script(src:="/static/konami.js"),
    link(rel:="icon", `type`:="image/png", href:="/static/favicon.jpg"),
    link(rel:="stylesheet", href:=s"/static/style.css"),
  )

  private def header(currentPage: String) = div(id:="header",
    p(`class`:="suites",
      Seq("heart", "club", "diamond", "spade").map(suite => img(src:=s"/static/$suite.png"))
    ),
    if (Details.underConstruction) h3("Website under construction - information subject to change") else frag(),
    h1(s"${Details.groom.split(" ").head} & ${Details.bride.split(" ").head}"),
    h3(s"${fullformat.format(Details.date)} • ${Details.location}"),
    h3({
      val days = ChronoUnit.DAYS.between(LocalDate.now(), Details.date)
      if (days > 0) s"$days days to go!"
      else "Today's the day! The sun is shining, the tank is clean!"
    }),
    div(id:="tabbar", for ((name, address) <- tabs) yield {
      a(`class`:=(if (name == currentPage) "tab underline" else "tab"), href:=address, name)
    })
  )

  private def footer() = div(id:="footer",
    h1(`class`:="underline", s"${Details.groom.head}&${Details.bride.head}"),
    shortformat.format(Details.date),
    p("Created from scratch"),
    p("Getting married? ", a(href:="https://github.com/ivoah/letsgetmarried", "Create your wedding website for free."))
  )

  private def page(name: String)(content: Frag*) = doctype("html")(html(
    _head(name),
    body(
      header(name),
      div(id:=name.toLowerCase.replace(" ", "_"), content),
      footer()
    )
  )).render

  def home(): String = page("Home")(
   img(src:=Details.hero),
    h2(s"The wedding of ${Details.groom} & ${Details.bride}"),
    h3(fullformat.format(Details.date)),
    for (location <- Details.locations) yield div(`class`:="location",
      h3(location.time),
      div(
        h3(location.name),
        a(href:=location.link, Markdown.render(location.address)),
        Markdown.render(location.details)
      )
    )
  )

  def story(): String = page("Our Story")(
    h2(Details.story.title),
    div(`class`:="markdown", Markdown.render(Details.story.body))
  )

  def party(): String = page("Wedding Party")(
    Details.bridesmaids.zip(Details.groomsmen).map { (bridesmaid, groomsman) => div(
      bridesmaid.render,
      groomsman.render
    )}
  )

  def registry(): String = page("Registry")(
    for (item <- Details.registry) yield a(href:=item.url, div(
      span(item.name),
      img(src:=item.image)
    ))
  )

  def rsvp(): String = page("RSVP")()
}
