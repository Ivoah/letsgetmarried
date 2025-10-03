package net.ivoah.letsgetmarried

import java.nio.file.Paths

import net.ivoah.vial.* 

class Endpoints() {
  def router: Router = Router {
    case ("GET" , "/", _) => Response(Templates.home())
    case ("GET", "/story", _) => Response(Templates.story())
    case ("GET", "/party", _) => Response(Templates.party())
    case ("GET", "/registry", _) => Response(Templates.registry())
    case ("GET", "/rsvp", _) => Response(Templates.rsvp())

    case ("GET", s"/static/$file", _) => Response.forFile(Paths.get("static"), Paths.get(file), None, Map("Cache-Control" -> Seq("max-age=3600")))
  }
}
