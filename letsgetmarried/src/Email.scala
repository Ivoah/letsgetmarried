package net.ivoah.letsgetmarried

import sttp.client4.quick.*
import com.typesafe.config.ConfigFactory

object Email {
  private val config = ConfigFactory.load()
  def sendEmails(subject: String, body: String): Either[String, String] = {
    val response = basicRequest
      .auth.basic("api", config.getString("mailgun.apiKey"))
      .body(Map(
        "from" -> s"Let's get married <noreply@${config.getString("domain")}>",
        "to" -> config.getStringList("emails").toArray.mkString(","),
        "subject" -> subject,
        "text" -> body
      ))
      .post(uri"https://api.mailgun.net/v3/${config.getString("mailgun.domain")}/messages")
      .send()

    response.body
  }
}
