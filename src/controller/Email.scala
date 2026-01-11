package net.ivoah.letsgetmarried
package controller

import sttp.client4.quick.*
import com.typesafe.config.Config
import scalatags.Text.all.*

object Email {
  def sendEmails(subject: String, content: Frag)(using config: Config): Unit = {
    val env = config.getString("env")
    if (env != "prod") return
    val prefix = if (env == "prod") "" else s"$env - "
    val response = basicRequest
      .auth.basic("api", config.getString("mailgun.apiKey"))
      .body(Map(
        "from" -> s"Let's get married <noreply@${config.getString("domain")}>",
        "to" -> config.getStringList("emails").toArray.mkString(","),
        "subject" -> (prefix + subject),
        "html" -> html(body(content)).render
      ))
      .post(uri"https://api.mailgun.net/v3/${config.getString("mailgun.domain")}/messages")
      .send()

    response.body.left.foreach(Console.err.println)
  }
}
