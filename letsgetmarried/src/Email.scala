package net.ivoah.letsgetmarried

import sttp.client4.quick.*
import com.typesafe.config.Config

object Email {
  def sendEmails(subject: String, body: String)(using config: Config): Either[String, String] = {
    val env = config.getString("env")
    val prefix = if (env == "prod") "" else s"$env - "
    val response = basicRequest
      .auth.basic("api", config.getString("mailgun.apiKey"))
      .body(Map(
        "from" -> s"Let's get married <noreply@${config.getString("domain")}>",
        "to" -> config.getStringList("emails").toArray.mkString(","),
        "subject" -> (prefix + subject),
        "text" -> body
      ))
      .post(uri"https://api.mailgun.net/v3/${config.getString("mailgun.domain")}/messages")
      .send()

    response.body
  }
}
