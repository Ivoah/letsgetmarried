package net.ivoah.letsgetmarried
package view

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

class Templates(details: model.Details, request: Request) {
  given Conversion[String, StringFrag] = (s: String) => StringFrag(
    if (request.cookies.exists(_.name == "scramble")) s.scramble
    else s
  )

  private val fullformat  = DateTimeFormatter.ofPattern("EEEE, MMMM d, yyyy")
  private val shortformat = DateTimeFormatter.ofPattern("M.d.y")
  private val weekday     = DateTimeFormatter.ofPattern("EEEE")
  private val timefmt     = DateTimeFormatter.ofPattern("h:mm a")

  private val divider = div(cls:="divider", (0 until 3).map(_ => img(src:="/static/diamond.svg")))

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
  ) ++ (if (details.hotels.nonEmpty) Seq("Hotels" -> "/hotels") else Seq())

  private def _head(_title: String) = head(
    title(s"${details.groom.split(" ").head} & ${details.bride.split(" ").head} - $_title"),
    meta(name:="viewport", content:="width=device-width", attr("initial-scale"):="1.0"),
    script(src:="/static/konami.js"),
    if (request.cookies.exists(_.name == "neko")) script(src:="/static/neko.js") else frag(),
    link(rel:="icon", `type`:="image/png", href:="/static/favicon.jpg"),
    link(rel:="stylesheet", href:=s"/static/style.css"),
    details.style.map(css => tag("style")(raw(css)))
  )

  private def _header(currentPage: String) = header(
    p(cls:="headerImages", details.headerImages.map(s => img(src:=s))),
    if (details.underConstruction) h3("Website under construction - information subject to change") else frag(),
    h1(s"${details.groom.split(" ").head} & ${details.bride.split(" ").head}"),
    h3(s"${fullformat.format(details.date)} • ${details.location}"),
    h3({
      val days = ChronoUnit.DAYS.between(LocalDate.now(), details.date)
      if (days == 0) "Today's the day! The sun is shining, the tank is clean!"
      else s"$days days to go!"
    }),
    div(id:="tabbar", for ((name, address) <- tabs) yield {
      a(cls:=(if (name == currentPage) "tab underline" else "tab"), href:=address, name)
    })
  )

  private def _footer() = footer(
    divider,
    h1(cls:="underline", s"${details.groom.head}&${details.bride.head}"),
    shortformat.format(details.date),
    p("Created from scratch"),
    p("Getting married? ", a(href:="https://github.com/ivoah/letsgetmarried", "Create your wedding website for free.")),
    p("up-up-down-down-left-right-left-right-b-a")
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
                td(input(id:=setting, `type`:="checkbox", attr("name"):=setting, if (request.cookies.exists(_.name == setting)) checked else frag())),
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

  def message(tab: String, msg: String): String = page(tab)(div(cls:="centered", Markdown.render(msg)))

  def home(): String = page("Home")(
    img(src:=details.image),
    h2(s"The wedding of ${details.groom} & ${details.bride}"),
    h3(fullformat.format(details.date)),
    for (location <- details.locations) yield div(cls:="location",
      h3(location.time),
      div(
        h3(location.name),
        a(href:=location.link, div(cls:="pre-wrap", location.address)),
        Markdown.render(location.details)
      )
    )
  )

  def story(): String = page("Our Story")(
    h2(details.story.title),
    img(src:=details.story.image),
    div(cls:="markdown", Markdown.render(details.story.body))
  )


  def party(): String = {
    def partyMember(member: model.PartyMember) = div(
      div(
        h3(member.name, br(), member.role),
        img(src:=member.image),
        div(cls:="markdown", Markdown.render(member.bio))
      )
    )

    page("Wedding Party")(
      details.bridesmaids.zip(details.groomsmen).map { (bridesmaid, groomsman) => div(
        partyMember(bridesmaid),
        partyMember(groomsman)
      )}
    )
  }

  def photos(): String = page("Photos")(
    details.photos.map { p =>
      figure(css("transform"):=s"rotate(${Random.between(-15.0, 15.0)}deg)",
        img(src:=p.image),
        div(figcaption(p.caption.map(Markdown.render(_))), a(href:=p.image, download:="", img(src:="/static/download.svg")))
      )
    },
    script(raw(
      """for (const figure of document.getElementsByTagName("figure")) {
        |  figure.addEventListener("mouseenter", e => e.target.style.transform = `rotate(${Math.random() * 30 - 15}deg)`);
        |}""".stripMargin
    ))
  )

  def registry(items: Seq[(model.RegistryItem, Boolean)], sortBy: String): String = page("Registry")(
    fieldset(
      legend("Please send all gifts to:"),
      div(cls:="centered", div(cls:="pre-wrap", details.registryAddress))
    ),
    Markdown.render(details.registryNotes),
    if (details.registry.isEmpty) frag()
    else frag(
      divider,
      "Sort by: ", Seq(
        ("None", ""),
        ("Price (low to high)", "priceLowHigh"),
        ("Price (high to low)", "priceHighLow"),
      ).flatMap {
        case (display, value) => Seq(a(href:=s"/registry?sortBy=$value#registry", display), frag(", "))
      }.init, // drop the trailing comma
      div(id:="registryItems",
        for ((item, purchased) <- items.sortBy(sortBy match {
          case "priceLowHigh" => _._1.price.getOrElse(Double.PositiveInfinity)
          case "priceHighLow" => -_._1.price.getOrElse(Double.PositiveInfinity)
          case _ => t => 0.0
        }).sortBy(_._2)) yield {
          frag(
            div(cls:="hoverGlow", onclick:=openDialog(item.id),
              div(cls:=(if (purchased) "disabled" else ""),
                img(src:=item.image),
                div(cls:="details",
                  span(item.name),
                  span(item.price.map(p => f"$$$p%.2f").getOrElse("$∞"))
                )
              ),
              if (purchased) img(src:="/static/purchased.svg", css("transform"):=s"rotate(${Random.between(-45.0, 45.0)}deg)") else frag()
            ),
            dialog(id:=s"${item.id}", div(
              input(`type`:="image", onclick:=closeDialog(item.id), src:="/static/close.svg"),
              div(
                p(item.name, item.price.map(p => f" - $$$p%.2f").getOrElse("")),
                img(src:=item.image),
                if (!purchased) a(cls:="button", href:=item.link, target:="_blank", s"Purchase at ${URI(item.link).getHost.split("\\.").takeRight(2).mkString(".")}") else frag(),
                input(`type`:="submit", value:=(if (purchased) "Unmark as given" else "Mark as given"), onclick:=openDialog(s"${item.id}-purchase")),
                dialog(id:=s"${item.id}-purchase", div(
                  input(`type`:="image", onclick:=closeDialog(s"${item.id}-purchase"), src:="/static/close.svg"),
                  div(
                    fieldset(
                      if (purchased) frag(
                        legend("Unmark as given"),
                        form(method:="POST", action:=s"/registry/delete/${item.id}",
                          label("Purchased by: ", input(`type`:="text", name:="purchasedBy")), br(),
                          input(`type`:="submit", value:="Unmark as given")
                        )
                      ) else frag(
                        legend("Mark as given"),
                        form(method:="POST", action:=s"/registry/${item.id}",
                          label("Purchased by: ", input(`type`:="text", name:="purchasedBy")), br(),
                          if (item.price.isEmpty) frag(label("Amount: ", input(`type`:="number", name:="amount", step:="0.01")), br()) else frag(),
                          label("Notes:", textarea(name:="notes")),
                          s"This does not buy the item, it only tells the bride and groom you have purchased it.",
                          input(`type`:="submit", value:="Mark as given")
                        )
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
    Markdown.render(details.rsvpNotes),
    if (details.invitations.isEmpty) frag()
    else form(action:="/rsvp", method:="GET",
      input(
        `type`:="search",
        name:="name",
        placeholder:="Full name",
      ),
      input(`type`:="submit", value:="Find your invitation")
    )
  )

  def rsvpFound(invitation: model.Invitation, rsvp: Option[model.RSVP]): String = page("RSVP", Some(s"RSVP for ${invitation.name}"))(
    form(action:="/rsvp", method:="POST",
      fieldset(
        legend(s"RSVP for ${invitation.name}"),
        "Who will be attending?",
        invitation.children match {
          case model.InviteStatus.Invited => " Please indicate how many children you are bringing and if they will need a seat at the table."
          case model.InviteStatus.NotInvited => " Due to space limitations we are only able to accommodate those listed on the invitation."
          case _ => frag()
        }, br(),
        input(`type`:="hidden", name:="name", value:=invitation.name),
        for (person <- invitation.people) yield frag(
          label(input(`type`:="checkbox", name:=person, if (rsvp.exists(_.people.contains(person))) checked else frag()), s" $person"), br()
        ),
        invitation.children match {
          case model.InviteStatus.Invited => frag(
            label("Children: ", input(`type`:="number", name:="children", min:=0, max:=9, value:=rsvp.map(_.children).getOrElse(0))), br(),
            label("Infants: ", input(`type`:="number", name:="infants", min:=0, max:=9, value:=rsvp.map(_.infants).getOrElse(0))), br(),
          )
          case _ => frag()
        },
        label("Regards:", textarea(name:="regards", rsvp.map(_.regards).getOrElse(""))),
        input(`type`:="submit", value:=s"${if (rsvp.nonEmpty) "Update" else "Save"} RSVP")
      )
    )
  )

  def hotels(): String = page("Hotels")(
    Markdown.render(details.hotelNotes),
    divider,
    details.hotels.map { hotel =>
      Markdown.render(s"${hotel.name}  \n[${hotel.address}](${hotel.link})")
    }
  )

  def admin(): String = page("Admin")(
    ul(
      li(a(href:="/admin/details", "Edit detials")),
      li(a(href:="/admin/rsvps", "RSVPs")),
      li(a(href:="/admin/gifts", "Gifts"))
    )
  )

  def rsvps(rsvps: Seq[model.RSVP]): String = page("RSVPs")(
    p(
      s"Adults: ${rsvps.map(_.people.length).sum}", br(),
      s"Children: ${rsvps.map(_.children).sum}", br(),
      s"Infants: ${rsvps.map(_.infants).sum}", br(),
      s"Total: ${rsvps.map(_.total).sum} (${rsvps.count(_.total > 0)})", br(),
      s"Outstanding: ${
        val outstanding = details.invitations.filter(i => !rsvps.exists(r => r.name == i.name))
        s"${outstanding.map(_.people.length).sum}${if (outstanding.exists(_.children == model.InviteStatus.Invited)) "+" else ""} (${outstanding.length})"
      }"
    ),
    for (invite <- details.invitations) yield {
      val rsvp = rsvps.find(_.name == invite.name)
      val coming = rsvp match {
        case Some(r) if r.total == 0 => "notComing"
        case Some(_) => "coming"
        case None => "noResponse"
      }
      tag("details")(
        tag("summary")(cls:=coming,
          span(invite.name),
          span(rsvp.map(_.total).getOrElse(0))
        ),
        rsvp.map(_.details).getOrElse(frag())
      )
    }
  )
  
  def gifts(allGifts: Seq[model.Gift]): String = page("Gifts")(
    p(s"Total: ${allGifts.length}"),
    for ((giver, gifts) <- allGifts.groupBy(_.purchasedBy).toSeq) yield {
      tag("details")(
        tag("summary")(
          span(giver),
          span(s"${gifts.length}")
        ),
        ul(gifts.map(gift => frag(
          li(attr("title"):=gift.id, s"${details.registry.find(_.id == gift.id).get.name}", gift.amount.map(g => s": $$$g").getOrElse("")),
          if (gift.notes.nonEmpty) p(gift.notes) else frag()
        )))
      )
    }
  )

  def editDetails() = page("Edit details")(
    form(method:="POST",
      ul(
        li(label(input(tpe:="checkbox", name:="underConstruction", if (details.underConstruction) checked else frag()), " Under construction")),
        li(label("Contact: ", input(name:="contact", value:=details.contact))),
        li(label("Style", textarea(name:="style", value:=details.style.getOrElse("")))),
        li("Header images", ul(
          for (img <- details.headerImages) yield li(input(tpe:="file", value:=img))
        )),
        li(label("Groom: ", input(name:="groom", value:=details.groom))),
        li(label("Bride: ", input(name:="bride", value:=details.bride))),
        li(label("Date: ", input(tpe:="date", name:="date", value:=details.date.toString))),
        li(label("Location: ", input(name:="location", value:=details.location))),
        li("Home tab", ul(
          li(label("Hero image: ", input(tpe:="file", name:="hero"))),
          li("Locations", ul(
            for ((location, i) <- details.locations.zipWithIndex) yield fieldset(
              legend(s"Location ${i + 1}"),
              ul(
                li(label("Name: ", input(name:="locationName", value:=location.name))),
                li(label("Time: ", input(name:="locationTime", value:=location.time))),
                li(label("Address: ", textarea(name:="locationAddress", value:=location.address))),
                li(label("Link: ", input(name:="locationLink", value:=location.link))),
                li(label("Details: ", input(name:="locationDetails", value:=location.details))),
              )
            )
          ))
        )),
        li("Story tab", ul(
          li(label("Title: ", input(name:="storyTitle", value:=details.story.title))),
        )),
      ),
      input(tpe:="submit", value:="Save")
    )
  )

  def invitation(inviteDetails: model.InvitationDetails): String = doctype("html")(html(
    head(
      link(rel:="stylesheet", href:=s"/static/style.css"),
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
                  h2(details.bride.head.toString),
                  h1(cls:="heart", "♥")
                ),
                div(id:="br", cls:="corner",
                  h2(details.groom.head.toString),
                  h1(cls:="heart", "♥")
                ),
                div(cls:="center",
                  div(
                    p(em(inviteDetails.tagline)),
                    divider,
                    p(s"${inviteDetails.parents} warmly invite you to the wedding of"),
                    p(
                      h2(details.bride),
                      h2("&"),
                      h2(details.groom),
                    ),
                    p(id:="date", weekday.format(details.date), span(shortformat.format(details.date)), timefmt.format(details.date)),
                    Markdown.render(inviteDetails.details),
                    divider,
                    p(
                      "Get details and RSVP at", br(),
                      a(href:=inviteDetails.url, em(inviteDetails.url)), br(),
                      s"Please RSVP by ${shortformat.format(inviteDetails.deadline)}"
                    )
                  )
                )
              )
            )
          )
        )
      ),
      div(id:="back",
        div(
          h2(details.bride.split(raw"\s+").map(_.head).mkString),
          h1("♥", lineHeight:=0.8),
          h2(details.groom.split(raw"\s+").map(_.head).mkString)
        )
      )
    )
  )).render

  def program(programDetails: model.ProgramDetails): String = {
    def people(title: String, names: Seq[String]) = div(
      strong(title), br(),
      names.map(n => frag(n, br()))
    )

    def schedule(schedule: Seq[Seq[String]]) = div(cls:="schedule",
      schedule.map {
        case Seq(p) => div(span(p))
        case Seq(p1, p2) => div(span(p1), span(cls:="antline"), span(p2))
      }
    )

    doctype("html")(html(
      head(
        link(rel:="stylesheet", href:=s"/static/style.css"),
        link(rel:="stylesheet", href:=s"/static/program.css"),
        link(rel:="icon", `type`:="image/png", href:="/static/favicon.jpg"),
        title("Program")
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
                  div(cls:="title", s"The Wedding Ceremony of", br(), s"${details.groom} & ${details.bride}"),
                  s"${fullformat.format(details.date)} - ${details.locations.find(_.name == "Ceremony").get.address.split("\n").head}",
                  schedule(programDetails.ceremony),
                  div(cls:="people",
                    div(
                      people("Matron of Honor", Seq(details.bridesmaids.head.name)),
                      people("Bridesmaids", details.bridesmaids.tail.map(_.name))
                    ),
                    div(
                      people("Best Man", Seq(details.groomsmen.head.name)),
                      people("Groomsmen", details.groomsmen.tail.map(_.name))
                    ),
                  ),
                  div(cls:="people",
                    people("Pastors", programDetails.pastors),
                    people("Pianist", Seq(programDetails.pianist)),
                    people("Ushers", programDetails.ushers),
                  ),
                  div(cls:="people",
                    people("Flower Girl", Seq(programDetails.flowerGirl)),
                    people("Ring Bearer", Seq(programDetails.ringBearer))
                  )
                )
              )
            )
          )
        ),
        div(id:="back",
          div(
            "Reception",
            Markdown.render(details.locations.find(_.name == "Reception").get.address),
            schedule(programDetails.reception)
          ),
          div(
            "Special Thanks",
            Markdown.render(programDetails.thanks)
          )
        )
      )
    )).render
  }

  def seating(): String = {
    doctype("html")(html(
      head(
        link(rel:="stylesheet", href:=s"/static/style.css"),
        link(rel:="stylesheet", href:=s"/static/seating.css"),
        link(rel:="icon", `type`:="image/png", href:="/static/favicon.jpg"),
        title("Seating labels")
      ),
      body(
        for (page <- model.Seating.grouped(3).grouped(10).toSeq) yield div(cls:="page",
          for (row <- page) yield div(cls:="row",
            for ((name, table) <- row) yield {
              val card = table.dropRight(1)
              val suit = table.takeRight(1)
              div(cls:="label",
                span(name), span(cls:="table", card, span(cls:=suit, suit))
              )
            }
          )
        )
      )
    )).render
  }
}

object Templates {
  val settings: Seq[String] = Seq("scramble", "neko")
}
