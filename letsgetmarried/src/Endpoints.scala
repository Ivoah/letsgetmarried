package net.ivoah.letsgetmarried

import java.nio.file.Paths

import net.ivoah.vial.*

class Endpoints() {
  def router: Router = Router {
    case ("GET" , "/", r) => Response(Templates(r).home())
    case ("GET", "/story", r) => Response(Templates(r).story())
    case ("GET", "/party", r) => Response(Templates(r).party())
    case ("GET", "/registry", r) => Response(Templates(r).registry(r.params.getOrElse("sortBy", "")))
    case ("GET", s"/rsvp", r) =>
      r.params.get("name") match {
        case Some(name) =>
          Details.invitees.find(_.name.equalsIgnoreCase(name)) match {
            case Some(invitee) => Response(Templates(r).rsvpFound(invitee, invitee.findRSVP()))
            case None => Response(Templates(r).rsvpNotFound(name))
          }
        case None => Response(Templates(r).rsvp())
      }
    case ("POST", s"/rsvp", r) =>
      r.form.expect("name", "infants", "children") { (name: String, infants: String, children: String) =>
          Details.invitees.find(_.name == name) match {
            case Some(invitee) =>
              val rsvp = RSVP(invitee.name, true, infants.toInt, children.toInt, Seq())
              rsvp.saveToDatabase()
              Response(Templates(r).rsvpSaved())
            case None =>
              Response.BadRequest()
          }
      }
    case ("POST", "/settings", r) =>
      Templates.settings.foldLeft(Response.Redirect(r.headers("Referer").head)) { (response, setting) =>
        response.withCookie(r.form.get(setting) match {
          case Some(v: String) => Cookie(setting, v)
          case _ => Cookie(setting, "", maxAge = Some(0))
        })
      }

//    case ("GET", s"/static/$file", _) => Response.forFile(Paths.get("static"), Paths.get(file), None, Map("Cache-Control" -> Seq("max-age=3600")))
    case ("GET", s"/static/$file", _) => Response.forFile(Paths.get("static"), Paths.get(file))
  }
}
