import org.scalajs.dom.*
import scala.scalajs.js.timers

extension [T](t: (T, T))(using n: Numeric[T]) {
  def +(other: (T, T)): (T, T) = {
    import n.*
    (t._1 + other._1, t._2 + other._2)
  }

  def -(other: (T, T)): (T, T) = {
    import n.*
    (t._1 - other._1, t._2 - other._2)
  }
}

case class Neko() {
  var position: (x: Int, y: Int) = Option(window.sessionStorage.getItem("neko.position"))
    .map { case s"($x,$y)" => (x.toInt, y.toInt)}
    .getOrElse(-32, -32)
  var target: (x: Int, y: Int) = Option(window.sessionStorage.getItem("neko.target"))
    .map { case s"($x,$y)" => (x.toInt, y.toInt)}
    .getOrElse(-32, -32)
  private var frame = 0

  private val speed = 15
  private val offsets: Seq[String] = Seq(
    "alert", "sleeping", "tail", "yawn",
    "paw-w", "paw-s", "paw-e", "paw-n",
    "run-nw", "run-w", "run-sw", "run-s", "run-se", "run-e", "run-ne", "run-n"
  )

  private def dx: Int = math.min(math.max(target.x - position.x, -speed), speed)
  private def dy: Int = math.min(math.max(target.y - position.y, -speed), speed)

  private def direction: String = Map(
    (-1, -1) -> "nw", ( 0, -1) -> "n", ( 1, -1) -> "ne",
    (-1,  0) -> "w",                   ( 1,  0) -> "e",
    (-1,  1) -> "sw", ( 0,  1) -> "s", ( 1,  1) -> "se",
  )(math.signum(dx), math.signum(dy))

  private def offset(state: String, frame: Int): String = {
    s"${if (frame == 0) 0 else -32}px ${offsets.indexOf(state) * -32}px"
  }

  val img: HTMLImageElement = document.createElement("img").asInstanceOf[HTMLImageElement]
  img.src = generatedDataUri
  img.style = """
    width: 32px;
    height: 32px;
    position: absolute;
    top: -32px;
    left: -32px;
    object-fit: none;
    object-position: 0px -32px;
    z-index: 420;
  """

  def tick(): Unit = {
    if (math.abs(dx) == 0 && math.abs(dy) == 0) {
      img.style.setProperty("object-position", offset("sleeping", frame))
      timers.setTimeout(750) { tick() }
    } else {
      img.style.setProperty("object-position", offset(s"run-$direction", frame))
      timers.setTimeout(250) { tick() }
    }
    frame = (frame + 1) % 2

    position = position + (dx, dy)
    img.style.left = s"${position.x - 16}px"
    img.style.top = s"${position.y - 16}px"
    window.sessionStorage.setItem("neko.position", position.toString)
    window.sessionStorage.setItem("neko.target", target.toString)
  }
}

@main
def main(): Unit = {
  document.addEventListener("DOMContentLoaded", _ => {
    val neko = Neko()
    document.body.appendChild(neko.img)
    document.addEventListener("mousemove", (e: MouseEvent) => {
      neko.target = (e.pageX.toInt, e.pageY.toInt)
      if (neko.position == (-32, -32)) neko.position = neko.target
    })

    var lastScroll = (0, 0)
    document.addEventListener("scroll", (e: UIEvent) => {
      val scroll = (e.view.scrollX.toInt, e.view.scrollY.toInt)
      neko.target = neko.target + (scroll - lastScroll)
      lastScroll = scroll
    })
    neko.tick()
  })
}
