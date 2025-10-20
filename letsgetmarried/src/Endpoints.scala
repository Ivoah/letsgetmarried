package net.ivoah.letsgetmarried

import java.nio.file.Paths

import net.ivoah.vial.*

extension [K, V]($: Map.type) {
  def unapplySeq(map: Map[K, V]): Option[Seq[(K, V)]] = Some(map.toSeq)
}

class Endpoints() {
  def router: Router = Router {
    case ("GET" , "/", _) => Response(Templates.home())
    case ("GET", "/story", _) => Response(Templates.story())
    case ("GET", "/party", _) => Response(Templates.party())
    case ("GET", "/registry", request) => Response(Templates.registry(request.params.getOrElse("sortBy", "")))
    case ("GET", s"/rsvp", request) =>
      request.params.get("name") match {
        case Some(name) =>
          Details.invitees.find(_.name.equalsIgnoreCase(name)) match {
            case Some(invitee) => Response(Templates.rsvpFound(invitee, invitee.findRSVP()))
            case None => Response(Templates.rsvpNotFound(name))
          }
        case None => Response(Templates.rsvp())
      }
    case ("POST", s"/rsvp", request) =>
      request.form.expect("name", "infants", "children") { (name: String, infants: String, children: String) =>
          Details.invitees.find(_.name == name) match {
            case Some(invitee) =>
              val rsvp = RSVP(invitee.name, true, infants.toInt, children.toInt, Seq())
              rsvp.saveToDatabase()
              Response(Templates.rsvpSaved())
            case None =>
              Response.BadRequest()
          }
      }

//    case ("GET", s"/static/$file", _) => Response.forFile(Paths.get("static"), Paths.get(file), None, Map("Cache-Control" -> Seq("max-age=3600")))
    case ("GET", s"/static/$file", _) => Response.forFile(Paths.get("static"), Paths.get(file))
  }
}
