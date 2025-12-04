package net.ivoah.letsgetmarried

import java.nio.file.Paths

import net.ivoah.vial.*

class Endpoints() {
  def router: Router = Router {
    case ("GET" , "/", r) => Response(Templates(r).home())
    case ("GET", "/story", r) => Response(Templates(r).story())
    case ("GET", "/party", r) => Response(Templates(r).party())
    case ("GET", "/photos", r) => Response(Templates(r).photos())
    case ("GET", "/registry", r) => Response(Templates(r).registry(r.params.getOrElse("sortBy", "")))
    case ("POST", s"/registry/$id", r) =>
      Details.registry.find(_.id == id) match {
        case Some(item) =>
          r.form.expect("purchasedBy") { (purchasedBy: String) =>
            (r.form.get("amount") match {
              case Some(amount: String) if amount.toDoubleOption.nonEmpty => Right(Some(amount.toDouble))
              case None => Right(None)
              case _ => Left(Response.BadRequest())
            }).map {amount =>
              if (item.purchase(purchasedBy, amount)) Response.Redirect("/registry")
              else Response.InternalServerError("Could not mark item as purchased")
            }.merge
          }
        case None => Response.NotFound()
      }
    case ("GET", s"/rsvp", r) =>
      r.params.get("name") match {
        case Some(name) =>
          Details.invitations.find(invite => (invite.people :+ invite.name).exists(_.equalsIgnoreCase(name.strip().split("\\s+").mkString(" ")))) match {
            case Some(invitation) => Response(Templates(r).rsvpFound(invitation, invitation.findRSVP()))
            case None => Response(Templates(r).rsvpNotFound(name))
          }
        case None => Response(Templates(r).rsvp())
      }
    case ("POST", s"/rsvp", r) =>
      val name = r.form("name").asInstanceOf[String]
      val adults = r.form("adults").asInstanceOf[String].toInt
      val children = r.form.get("children").map(_.asInstanceOf[String].toInt).getOrElse(0)
      val infants = r.form.get("infants").map(_.asInstanceOf[String].toInt).getOrElse(0)
      Details.invitations.find(_.name == name) match {
        case Some(invitation) =>
          val rsvp = RSVP(invitation.name, adults, children, infants)
          if (rsvp.saveToDatabase()) {
            Email.sendEmails(s"Received RSVP for $name", s"Received RSVP for $name.\n\nAdults: $adults\nChildren: $children\nInfants: $infants").left.foreach(println)
            Response(Templates(r).rsvpSaved())
          } else {
            throw Exception("Could not save RSVP")
          }
        case None =>
          Response.BadRequest()
      }
    case ("POST", "/settings", r) =>
      Templates.settings.foldLeft(Response.Redirect(r.headers.get("Referer").map(_.head).getOrElse("/"))) { (response, setting) =>
        response.withCookie(r.form.get(setting) match {
          case Some(v: String) => Cookie(setting, v)
          case _ => Cookie(setting, "", maxAge = Some(0))
        })
      }

    case ("GET", "/invitation", r) => Response(Templates(r).invitation())
//    case ("GET", s"/static/$file", _) => Response.forFile(Paths.get("static"), Paths.get(file), None, Map("Cache-Control" -> Seq("max-age=3600")))
    case ("GET", s"/static/$file", _) => Response.forFile(Paths.get("static"), Paths.get(file))
    case ("GET", s"/photos/$file", _) => Response.forFile(Paths.get("photos"), Paths.get(file), None, Map("Cache-Control" -> Seq("max-age=3600")))
  }
}
