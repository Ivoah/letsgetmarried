package net.ivoah.letsgetmarried

import scala.util.Random
import java.time.LocalDate
import java.time.temporal.ChronoUnit
import scalatags.Text.all.*
import scalatags.Text.tags2.title
import net.ivoah.vial.Request

import java.net.URI
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

  private val fullformat  = DateTimeFormatter.ofPattern("EEEE, MMMM d, yyyy")
  private val shortformat = DateTimeFormatter.ofPattern("M.d.y")
  private val weekday     = DateTimeFormatter.ofPattern("EEEE")
  private val timefmt     = DateTimeFormatter.ofPattern("h:mm a")

  private val dialog = tag("dialog")(attr("closedby"):="any")
  private def openDialog(id: String) = s"""document.getElementById("$id").showModal(); return false;"""
  private def closeDialog(id: String) = s"""document.getElementById("$id").close(); return false;"""

  private val tabs = Seq(
    "Home" -> "/",
    "Our Story" -> "/story",
    "Wedding Party" -> "/party",
    "Photos" -> "/photos",
    "Registry" -> "/registry",
    "RSVP" -> "/rsvp"
  )

  private def _head(_title: String) = head(
    title(s"${Details.groom.split(" ").head} & ${Details.bride.split(" ").head} - $_title"),
    meta(name:="viewport", content:="width=device-width", attr("initial-scale"):="1.0"),
    script(src:="/static/konami.js"),
    if (request.cookies.exists(_.name == "mazda")) link(rel:="stylesheet", href:="/static/mazda.css") else frag(),
    link(rel:="icon", `type`:="image/png", href:="/static/favicon.jpg"),
    link(rel:="stylesheet", href:=s"/static/style.css"),
  )

  private def _header(currentPage: String) = header(
    p(cls:="suites",
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
      a(cls:=(if (name == currentPage) "tab underline" else "tab"), href:=address, name)
    })
  )

  private def _footer() = footer(
    h1(cls:="underline", s"${Details.groom.head}&${Details.bride.head}"),
    shortformat.format(Details.date),
    p("Created from scratch"),
    p("Getting married? ", a(href:="https://github.com/ivoah/letsgetmarried", "Create your wedding website for free."))
  )

  private def page(name: String, _title: Option[String] = None)(content: Frag*) = doctype("html")(html(
    _head(_title.getOrElse(name)),
    body(
      dialog(id:="settings",
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
    for (location <- Details.locations) yield div(cls:="location",
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
    div(cls:="markdown", Markdown.render(Details.story.body))
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
        div(figcaption(p.caption.map(Markdown.render(_)).getOrElse(frag())), a(href:=p.image, download:="", img(src:="/static/download.svg")))
      )
    },
    script(raw(
      """for (const figure of document.getElementsByTagName("figure")) {
        |  figure.addEventListener("mouseenter", e => e.target.style.transform = `rotate(${Math.random() * 30 - 15}deg)`);
        |}""".stripMargin
    ))
  )

  def registry(sortBy: String): String = page("Registry")(
    if (Details.registry.isEmpty) p(textAlign.center, "Coming soon!")
    else frag(
      fieldset(
        legend("Please send all packages to:"),
        div(cls:="centered", Markdown.render(Details.registryAddress))
      ),
      "Sort by: ", Seq(
        ("None", "none"),
        ("Price (low to high)", "priceLowHigh"),
        ("Price (high to low)", "priceHighLow"),
      ).flatMap {
        case (display, value) => Seq(a(href:=s"/registry?sortBy=$value#registry", display), frag(", "))
      }.init,
      div(id:="registryItems",
        for (item <- Details.registry.sortBy(sortBy match {
          case "priceLowHigh" => (i: RegistryItem) => i.price.getOrElse(Double.PositiveInfinity)
          case "priceHighLow" => (i: RegistryItem) => -i.price.getOrElse(Double.PositiveInfinity)
          case _ => (i: RegistryItem) => 0.0 //-(i.count - i.purchased()).toDouble
        })) yield {
          frag(
            div(cls:="hoverGlow", onclick:=openDialog(item.id),
              div(cls:=(if (item.purchased()) "disabled" else ""),
                img(src:=item.image),
                div(cls:="details",
                  span(item.name),
                  span(item.price.map(p => f"$$$p%.2f").getOrElse("$∞"))
                )
              ),
              if (item.purchased()) img(src:="/static/purchased.svg", css("transform"):=s"rotate(${Random.between(-45.0, 45.0)}deg)") else frag()
            ),
            dialog(id:=s"${item.id}", div(
              input(`type`:="image", onclick:=closeDialog(item.id), src:="/static/close.svg"),
              div(
                p(item.name),
                img(src:=item.image),
                a(cls:="button", href:=item.link, target:="_blank", s"Purchase at ${URI(item.link).getHost.split("\\.").takeRight(2).mkString(".")}"),
                input(`type`:="submit", value:="Mark as purchased", onclick:=openDialog(s"${item.id}-purchase")),
                dialog(id:=s"${item.id}-purchase", div(
                  input(`type`:="image", onclick:=closeDialog(s"${item.id}-purchase"), src:="/static/close.svg"),
                  div(
                    fieldset(
                      legend("Mark as purchased"),
                      form(method:="POST", action:=s"/registry/${item.id}",
                        label("Purchased by: ", input(`type`:="text", name:="purchasedBy")), br(),
                        if (item.price.isEmpty) frag(label("Amount: ", input(`type`:="number", name:="amount", step:="0.01")), br()) else frag(),
                        s"This does not buy the item, it only tells the bride and groom you have purchased it. Please make sure you have entered the information correctly. This action cannot be undone.",
                        input(`type`:="submit", value:="Mark as purchased")
                      )
                    )
                  )
                ))
              )
            ))
          )
        }
      )
    )
  )

  def rsvp(): String = page("RSVP")(
    if (Details.invitations.isEmpty) {
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

  def rsvpFound(invitation: Invitation, rsvp: Option[RSVP]): String = page("RSVP", Some(s"RSVP for ${invitation.name}"))(
    form(action:="/rsvp", method:="POST",
      fieldset(
        legend(s"RSVP for ${invitation.name}"),
        "Who will be attending?",
        invitation.children match {
          case InviteStatus.Invited => " Please indicate how many children you are bringing and if they will need a seat at the table."
          case InviteStatus.NotInvited => " Due to space limitations we are only able to accommodate those listed on the invitation."
          case _ => frag()
        }, br(),
        input(`type`:="hidden", name:="name", value:=invitation.name),
        for (person <- invitation.people) yield frag(
          label(input(`type`:="checkbox", name:=person, if (rsvp.exists(_.people.contains(person))) checked else frag()), s" $person"), br()
        ),
        invitation.children match {
          case InviteStatus.Invited => frag(
            label("Children: ", input(`type`:="number", name:="children", min:=0, max:=9, value:=rsvp.map(_.children).getOrElse(0))), br(),
            label("Infants: ", input(`type`:="number", name:="infants", min:=0, max:=9, value:=rsvp.map(_.infants).getOrElse(0))),
          )
          case _ => frag()
        },
        input(`type`:="submit", value:="Save RSVP")
      )
    )
  )

  def rsvpNotFound(name: String): String = page("RSVP")(
    p(s"Could not find an invitation for $name. Please make sure you entered your full first and last name as it appears on your invitation. Contact ", a(href:=s"mailto:${Details.contact}", Details.contact), " if you believe this is in error.")
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
        div(id:="b1", cls:="border",
          div(id:="b2", cls:="border",
            div(id:="b3", cls:="border",
              div(cls:="gridBorder", css("grid-area"):="n", (0 until 25).map(_ => img(src:="static/diamond.svg"))),
              div(cls:="gridBorder", css("grid-area"):="s", (0 until 25).map(_ => img(src:="static/diamond.svg"))),
              div(cls:="gridBorder side", css("grid-area"):="e", (0 until 35).map(_ => img(src:="static/diamond.svg"))),
              div(cls:="gridBorder side", css("grid-area"):="w", (0 until 35).map(_ => img(src:="static/diamond.svg"))),
              div(id:="b4", cls:="border",
                div(id:="tl", cls:="corner",
                  h2(Details.bride.head.toString),
                  h1(cls:="heart", "♥")
                ),
                div(id:="br", cls:="corner",
                  h2(Details.groom.head.toString),
                  h1(cls:="heart", "♥")
                ),
                div(cls:="center",
                  div(
                    p(em(Details.invitationDetails.tagline)),
                    div((0 until 3).map(_ => img(src:="/static/diamond.svg"))),
                    p(s"${Details.invitationDetails.parents} warmly invite you to the wedding of"),
                    p(
                      h2(Details.bride),
                      h2("&"),
                      h2(Details.groom),
                    ),
                    p(id:="date", weekday.format(Details.date), span(shortformat.format(Details.date)), timefmt.format(Details.date)),
                    Markdown.render(Details.invitationDetails.details),
                    div((0 until 3).map(_ => img(src:="/static/diamond.svg"))),
                    p("Get details and RSVP at", br(), em(Details.invitationDetails.url), br(), s"Please RSVP by ${shortformat.format(Details.invitationDetails.deadline)}")
                  )
                )
              )
            )
          )
        )
      ),
      div(id:="back",
        div(
          h2(Details.bride.split(raw"\s+").map(_.head).mkString),
          h1("♥", lineHeight:=0.8),
          h2(Details.groom.split(raw"\s+").map(_.head).mkString)
        )
      )
    )
  )).render
}

object Templates {
  val settings = Seq("mazda", "scramble")
}
