package net.ivoah.letsgetmarried

import scala.util.Random
import java.time.LocalDate
import java.time.temporal.ChronoUnit
import scalatags.Text.all.*
import scalatags.Text.tags2.title
import net.ivoah.vial.Request

import java.time.format.DateTimeFormatter

val words = raw"\w+".r
extension (s: String) {
  def scramble: String = words.replaceAllIn(s, m => {
    val w = m.matched
    if (w.length >= 3) s"${w.head}${util.Random.shuffle(w.drop(1).dropRight(1))}${w.last}"
    else w
  })
}

class Templates(request: Request) {
  given Conversion[String, StringFrag] = (s: String) => StringFrag(
    if (request.cookies.exists(_.name == "scramble")) s.scramble
    else s
  )

  private val tabs = Seq(
    "Home" -> "/",
    "Our Story" -> "/story",
    "Wedding Party" -> "/party",
    "Photos" -> "/photos",
    "Registry" -> "/registry",
    "RSVP" -> "/rsvp"
  )

  private val fullformat  = DateTimeFormatter.ofPattern("EEEE, MMMM d, yyyy")
  private val shortformat = DateTimeFormatter.ofPattern("M.d.y")
  private val weekday     = DateTimeFormatter.ofPattern("EEEE")
  private val timefmt     = DateTimeFormatter.ofPattern("h:mm a")

  private def _head(_title: String) = head(
    title(s"${Details.groom.split(" ").head} & ${Details.bride.split(" ").head} - $_title"),
    meta(name:="viewport", content:="width=device-width", attr("initial-scale"):="1.0"),
    script(src:="/static/konami.js"),
    if (request.cookies.exists(_.name == "mazda")) link(rel:="stylesheet", href:="/static/mazda.css") else frag(),
    link(rel:="icon", `type`:="image/png", href:="/static/favicon.jpg"),
    link(rel:="stylesheet", href:=s"/static/style.css"),
  )

  private def _header(currentPage: String) = header(
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

  private def _footer() = footer(
    h1(`class`:="underline", s"${Details.groom.head}&${Details.bride.head}"),
    shortformat.format(Details.date),
    p("Created from scratch"),
    p("Getting married? ", a(href:="https://github.com/ivoah/letsgetmarried", "Create your wedding website for free."))
  )

  private def page(name: String)(content: Frag*) = doctype("html")(html(
    _head(name),
    body(
      tag("dialog")(id:="settings", attr("closedby"):="any",
        form(method:="POST", action:="/settings",
          fieldset(
            legend("Super secret settings"),
            table(
              for (setting <- Templates.settings) yield tr(
                td(input(`type`:="checkbox", `id`:=setting, attr("name"):=setting, if (request.cookies.exists(_.name == setting)) checked else frag())),
                td(label(`for`:=setting, setting.capitalize))
              )
            ),
            button("Save settings")
          )
        )
      ),
      _header(name),
      div(id:=name.toLowerCase.replace(" ", "_"), content),
      _footer()
    )
  )).render

  def home(): String = page("Home")(
   img(src:=Details.image),
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
    img(src:=Details.story.image),
    div(`class`:="markdown", Markdown.render(Details.story.body))
  )

  private def partyMember(member: PartyMember) = div(
    div(
      h3(member.name, br(), member.role),
      img(src:=member.image),
      Markdown.render(member.bio)
    )
  )

  def party(): String = page("Wedding Party")(
    Details.bridesmaids.zip(Details.groomsmen).map { (bridesmaid, groomsman) => div(
      partyMember(bridesmaid),
      partyMember(groomsman)
    )}
  )

  def photos(): String = page("Photos")(
    Details.photos.map { p =>
      figure(css("transform"):=s"rotate(${Random.between(-15.0, 15.0)}deg)",
        img(src:=p.image),
        div(figcaption(raw(p.caption)), a(href:=p.image, download:="", img(src:="/static/download.svg")))
      )
    },
    script(raw(
      """for (const figure of document.getElementsByTagName("figure")) {
        |  figure.addEventListener("mouseenter", e => e.target.style.transform = `rotate(${Math.random() * 30 - 15}deg)`);
        |}""".stripMargin))
  )

  def registry(sortBy: String): String = page("Registry")(
    if (Details.registry.isEmpty) {
      p(textAlign.center, "Coming soon!")
    } else {
      frag(
        "Sort by: ", Seq(
          ("None", "none"),
          ("Price (low to high)", "priceLowHigh"),
          ("Price (high to low)", "priceHighLow"),
        ).flatMap {
          case (display, value) => Seq(a(href:=s"/registry?sortBy=$value", display), frag(", "))
        }.init,
        div(id:="registryItems",
          for (item <- Details.registry.sortBy(sortBy match {
            case "priceLowHigh" => _.price
            case "priceHighLow" => -_.price
            case _ => _ => 0.0
          })) yield div(a(href:=item.link,
            img(src:=item.image),
            span(item.name),
            span(s"$$${item.price}")
          ))
        )
      )
    }
  )

  def rsvp(): String = page("RSVP")(
    if (Details.invitees.isEmpty) {
      p(textAlign.center, "Coming soon!")
    } else {
      form(action:="/rsvp", method:="GET",
        input(
          `type`:="search",
          name:="name",
          placeholder:="Full name",
        ),
        input(`type`:="submit", value:="Find your invitation")
      )
    }
  )

  def rsvpFound(invitee: Invitee, rsvp: Option[RSVP]): String = page("RSVP")(
    form(action:="/rsvp", method:="POST",
      fieldset(
        legend(s"RSVP for ${invitee.name}"),
        input(`type`:="hidden", name:="name", value:=invitee.name),
        label("Attending?", input(`type`:="checkbox", name:="attending", if (rsvp.exists(_.attending)) checked else frag())), br(),
        label("Number of infants: ", input(`type`:="number", name:="infants", value:=rsvp.map(_.infants).getOrElse(0))), br(),
        label("Number of children: ", input(`type`:="number", name:="children", value:=rsvp.map(_.children).getOrElse(0))),
        fieldset(
          legend("Also RSVP for"),
          invitee.linked.map { linked =>
            label(input(`type`:="checkbox", name:=linked), linked)
          }
        ),
        input(`type`:="submit", value:="Save RSVP")
      )
    )
  )

  def rsvpNotFound(name: String): String = page("RSVP")(
    p(s"Could not find an invitation for $name. Please make sure you entered your full first and last name.")
  )

  def rsvpSaved(): String = page("RSVP")(
    p("Thank you! Your RSVP has been saved.")
  )

  def invitation(): String = doctype("html")(html(
    head(
      link(rel:="stylesheet", href:=s"/static/style.css"),
      if (request.cookies.exists(_.name == "mazda")) link(rel:="stylesheet", href:="/static/mazda.css") else frag(),
      link(rel:="stylesheet", href:=s"/static/invitation.css"),
      link(rel:="icon", `type`:="image/png", href:="/static/favicon.jpg"),
      title("Invitation")
    ),
    body(
      div(id:="front",
        div(id:="b1", `class`:="border",
          div(id:="b2", `class`:="border",
            div(id:="b3", `class`:="border",
              div(id:="b4", `class`:="border",
                div(id:="tl", `class`:="corner",
                  h2(Details.bride.head.toString),
                  h1(`class`:="heart", "♥")
                ),
                div(id:="br", `class`:="corner",
                  h2(Details.groom.head.toString),
                  h1(`class`:="heart", "♥")
                ),
                div(`class`:="center",
                  div(
                    p(em(Details.invitation.tagline)),
                    div((0 until 3).map(_ => img(src:="/static/diamond.svg"))),
                    p(s"${Details.invitation.parents} warmly invite you to the wedding of"),
                    p(
                      h2(Details.bride),
                      h2("&"),
                      h2(Details.groom),
                    ),
                    p(id:="date", weekday.format(Details.date), span(shortformat.format(Details.date)), timefmt.format(Details.date)),
                    Markdown.render(Details.invitation.details),
                    div((0 until 3).map(_ => img(src:="/static/diamond.svg"))),
                    p("Get details and RSVP at", br(), em(Details.invitation.url))
                  )
                )
              )
            )
          )
        )
      ),
      div(id:="back",
        div(
          h2("LRM"),
          h1("♥", lineHeight:=0.8),
          h2("NMR")
        )
      )
    )
  )).render
}

object Templates {
  val settings = Seq("mazda", "scramble")
}
