package net.ivoah.letsgetmarried
package controller

import com.typesafe.config.{Config, ConfigFactory}
import net.ivoah.vial.*
import scalatags.Text.all.*

import java.nio.file.Paths
import java.time.LocalDate
import scala.util.Try

class Endpoints(details: model.Details) {
  given Config = ConfigFactory.load()

  def router: Router = Router {
    case ("GET" , "/", r) => Response(view.Templates(details, r).home())
    case ("GET", "/story", r) => Response(view.Templates(details, r).story())
    case ("GET", "/party", r) => Response(view.Templates(details, r).party())
    case ("GET", "/photos", r) => Response(view.Templates(details, r).photos())
    case ("GET", "/registry", r) =>
      val items = details.registry.items.map(item => item -> model.Database.getRegistryItemPurchase(item).nonEmpty)
      Response(view.Templates(details, r).registry(items, r.params.getOrElse("sortBy", "")))
    // POST /registry/delete/$id is before POST /registry/$id so it matches first
    case ("POST", s"/registry/delete/$id", r) =>
      println(id)
      details.registry.items.find(_.id == id) match {
        case Some(item) =>
          r.form.expect("purchasedBy") { (purchasedBy: String) =>
            if (model.Database.getRegistryItemPurchase(item).contains(purchasedBy)) {
              if (model.Database.removeRegistryItemPurchase(item)) {
                Email.sendEmails(s"$purchasedBy unmarked something as purchased on the registry", s"$purchasedBy no longer bought \"${item.name}\".")
                Response(view.Templates(details, r).message("Registry", s"${item.name} is no longer marked as purchased."))
              } else Response(view.Templates(details, r).message("Registry", "Could not unmark item as purchased"), status_code = 500)
            } else {
              Response(view.Templates(details, r).message("Registry", "Could not unmark item as purchased. Make sure you entered the name exactly as when you marked the item as purchased."), status_code = 400)
            }
          }.getOrElse(Response.BadRequest())
        case None => Response.NotFound()
      }
    case ("POST", s"/registry/$id", r) =>
      details.registry.items.find(_.id == id) match {
        case Some(item) =>
          r.form.expect("purchasedBy", "notes") { (purchasedBy: String, notes: String) =>
            ((purchasedBy, r.form.get("amount")) match {
              case ("", _) => Left(Response(view.Templates(details, r).message("Registry", "Please indicate who purchased the gift."), status_code = 400))
              case (_, Some(amount: String)) if amount.toDoubleOption.nonEmpty => Right(Some(amount.toDouble))
              case (_, None) => Right(None)
              case _ => Left(Response(view.Templates(details, r).message("Registry", "amount field must be a valid number if provided"), status_code = 400))
            }).map { amount =>
              if (model.Database.addRegistryItemPurchase(item, purchasedBy, amount, notes)) {
                Email.sendEmails(
                  s"$purchasedBy bought something off the registry!",
                  html(body(p(s"$purchasedBy just bought \"${item.name}\"${amount.map(m => s" ($$$m)").getOrElse("")}."), p(notes)))
                )
                Response(view.Templates(details, r).message("Registry", "Thank you! Your gift has been recorded."))
              } else Response(view.Templates(details, r).message("Registry", "Could not mark item as purchased"), status_code = 500)
            }.merge
          }.getOrElse(Response.BadRequest())
        case None => Response.NotFound()
      }
    case ("GET", s"/rsvp", r) =>
      r.params.get("name") match {
        case Some(name) =>
          details.rsvp.invitations.find(invite => (invite.name +: invite.people).exists(_.equalsIgnoreCase(name.strip().split("\\s+").mkString(" ")))) match {
            case Some(invitation) => Response(view.Templates(details, r).rsvpFound(invitation, model.Database.findRSVP(invitation.name)))
            case None => Response(view.Templates(details, r).message("RSVP", s"Could not find an invitation for \"$name\". Please make sure you entered your full first and last name as it appears on your invitation. Contact [${details.general.contact}](mailto:${details.general.contact}) if you believe this is in error."), status_code = 404)
          }
        case None => Response(view.Templates(details, r).rsvp())
      }
    case ("POST", s"/rsvp", r) =>
      println(r.form)
      r.form.expect("name", "regards") { (name: String, regards: String) =>
        val children = r.form.get("children").map(_.asInstanceOf[String].toInt).getOrElse(0)
        val infants = r.form.get("infants").map(_.asInstanceOf[String].toInt).getOrElse(0)
        details.rsvp.invitations.find(_.name == name) match {
          case Some(invitation) =>
            val people = invitation.people.filter(r.form.contains)
            val rsvp = model.RSVP(invitation.name, people, children, infants, regards)
            if (model.Database.saveRSVP(rsvp)) {
              Email.sendEmails(s"Received RSVP for $name", rsvp.details)
              Response(view.Templates(details, r).message("RSVP", "Thank you! Your RSVP has been saved."))
            } else Response(view.Templates(details, r).message("RSVP", "Could not save RSVP"), status_code = 500)
          case None =>
            Response(view.Templates(details, r).message("RSVP", s"Could not find invitation for \"${name}\""), status_code = 404)
        }
      }.getOrElse(Response.BadRequest())
    case ("GET", "/hotels", r) => Response(view.Templates(details, r).hotels())
    case ("POST", "/settings", r) =>
      view.Templates.settings.foldLeft(Response.Redirect(r.headers.get("Referer").map(_.head).getOrElse("/"))) { (response, setting) =>
        response.withCookie(r.form.get(setting) match {
          case Some(v: String) => Cookie(setting, v)
          case _ => Cookie(setting, "", maxAge = Some(0))
        })
      }

    case ("GET", "/admin", r) => Response(view.Templates(details, r).admin())
    case ("GET", "/admin/details", r) => Response(view.Templates(details, r).editDetails())
    case ("POST", "/admin/details", r) =>
      println(r.form)
      println(model.Details.fromForm(r.form))
      Response.Redirect("/admin/details")
    case ("GET", "/admin/rsvps", r) => Response(view.Templates(details, r).rsvps(model.Database.getAllRSVPs()))
    case ("GET", "/admin/gifts", r) => Response(view.Templates(details, r).gifts(model.Database.getAllGifts()))

    case ("GET", "/invitation", r) => Response(view.Templates(details, r).invitation())
    case ("GET", "/program", r) => Response(view.Templates(details, r).program())
    case ("GET", "/seating", r) => Response(view.Templates(details, r).seating())
    // case ("GET", s"/static/$file", _) => Response.forFile(Paths.get("static"), Paths.get(file), None, Map("Cache-Control" -> Seq("max-age=3600")))
    case ("GET", s"/static/$file", _) => Response.forFile(Paths.get("static"), Paths.get(file))
    case ("GET", s"/photos/$file", _) => Response.forFile(Paths.get("photos"), Paths.get(file), None, Map("Cache-Control" -> Seq("max-age=3600")))
  }
}
