package net.ivoah.letsgetmarried
package controller

import com.typesafe.config.{Config, ConfigFactory}
import net.ivoah.vial.*
import scalatags.Text.all.*

import java.nio.file.Paths
import java.time.LocalDate
import scala.util.Try

class Endpoints {
  given Config = ConfigFactory.load()

  def router: Router = Router {
    case ("GET" , "/", r) => Response(view.Templates(r).home())
    case ("GET", "/story", r) => Response(view.Templates(r).story())
    case ("GET", "/party", r) => Response(view.Templates(r).party())
    case ("GET", "/photos", r) => Response(view.Templates(r).photos())
    case ("GET", "/registry", r) =>
      val items = model.Details.registry.map(item => item -> model.Database.getRegistryItemPurchase(item).nonEmpty)
      Response(view.Templates(r).registry(items, r.params.getOrElse("sortBy", "")))
    // POST /registry/delete/$id is before POST /registry/$id so it matches first
    case ("POST", s"/registry/delete/$id", r) =>
      println(id)
      model.Details.registry.find(_.id == id) match {
        case Some(item) =>
          r.form.expect("purchasedBy") { (purchasedBy: String) =>
            if (model.Database.getRegistryItemPurchase(item).contains(purchasedBy)) {
              if (model.Database.removeRegistryItemPurchase(item)) {
                Email.sendEmails(s"$purchasedBy unmarked something as purchased on the registry", s"$purchasedBy no longer bought \"${item.name}\".")
                Response(view.Templates(r).registryDeleted(item))
              } else Response.InternalServerError("Could not unmark item as purchased")
            } else {
              Response.BadRequest("Could not unmark item as purchased. Make sure you entered the name exactly as when you marked the item as purchased.")
            }
          }
        case None => Response.NotFound()
      }
    case ("POST", s"/registry/$id", r) =>
      model.Details.registry.find(_.id == id) match {
        case Some(item) =>
          r.form.expect("purchasedBy", "notes") { (purchasedBy: String, notes: String) =>
            (r.form.get("amount") match {
              case Some(amount: String) if amount.toDoubleOption.nonEmpty => Right(Some(amount.toDouble))
              case None => Right(None)
              case _ => Left(Response.BadRequest())
            }).map { amount =>
              if (model.Database.addRegistryItemPurchase(item, purchasedBy, amount, notes)) {
                Email.sendEmails(
                  s"$purchasedBy bought something off the registry!",
                  html(body(p(s"$purchasedBy just bought \"${item.name}\"${amount.map(m => s" ($$$m)").getOrElse("")}."), p(notes)))
                )
                Response(view.Templates(r).registrySaved())
              } else Response.InternalServerError("Could not mark item as purchased")
            }.merge
          }
        case None => Response.NotFound()
      }
    case ("GET", s"/rsvp", r) =>
      r.params.get("name") match {
        case Some(name) =>
          model.Details.invitations.find(invite => (invite.name +: invite.people).exists(_.equalsIgnoreCase(name.strip().split("\\s+").mkString(" ")))) match {
            case Some(invitation) => Response(view.Templates(r).rsvpFound(invitation, model.Database.findRSVP(invitation.name)))
            case None => Response(view.Templates(r).rsvpNotFound(name))
          }
        case None => Response(view.Templates(r).rsvp())
      }
    case ("POST", s"/rsvp", r) =>
      println(r.form)
      r.form.expect("name", "regards") { (name: String, regards: String) =>
        val children = r.form.get("children").map(_.asInstanceOf[String].toInt).getOrElse(0)
        val infants = r.form.get("infants").map(_.asInstanceOf[String].toInt).getOrElse(0)
        model.Details.invitations.find(_.name == name) match {
          case Some(invitation) =>
            val people = invitation.people.filter(r.form.contains)
            val rsvp = model.RSVP(invitation.name, people, children, infants, regards)
            if (model.Database.saveRSVP(rsvp)) {
              Email.sendEmails(s"Received RSVP for $name", rsvp.details)
              Response(view.Templates(r).rsvpSaved())
            } else Response.InternalServerError("Could not save RSVP")
          case None =>
            Response.BadRequest()
        }
      }
    case ("GET", "/hotels", r) => Response(view.Templates(r).hotels())
    case ("POST", "/settings", r) =>
      view.Templates.settings.foldLeft(Response.Redirect(r.headers.get("Referer").map(_.head).getOrElse("/"))) { (response, setting) =>
        response.withCookie(r.form.get(setting) match {
          case Some(v: String) => Cookie(setting, v)
          case _ => Cookie(setting, "", maxAge = Some(0))
        })
      }

    case ("GET", "/admin", r) => Response(view.Templates(r).admin())
    case ("GET", "/admin/rsvps", r) => Response(view.Templates(r).rsvps(model.Database.getAllRSVPs()))
    case ("GET", "/admin/gifts", r) => Response(view.Templates(r).gifts(model.Database.getAllGifts()))

    case ("GET", "/invitation", r) => Response(view.Templates(r).invitation())
    // case ("GET", s"/static/$file", _) => Response.forFile(Paths.get("static"), Paths.get(file), None, Map("Cache-Control" -> Seq("max-age=3600")))
    case ("GET", s"/static/$file", _) => Response.forFile(Paths.get("static"), Paths.get(file))
    case ("GET", s"/photos/$file", _) => Response.forFile(Paths.get("photos"), Paths.get(file), None, Map("Cache-Control" -> Seq("max-age=3600")))
  }
}
