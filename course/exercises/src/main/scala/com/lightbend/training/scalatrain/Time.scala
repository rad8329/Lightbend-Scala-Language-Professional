package com.lightbend.training.scalatrain

import play.api.libs.json.{JsValue, Json}

import scala.util.Try

object Time {
  def fromMinutes(minutes: Int): Time = {
    val normalizaedHours: Int = minutes / 60
    val normalizedMinutes: Int = minutes % 60

    Time(normalizaedHours, normalizedMinutes)
  }

  def fromJson(json: JsValue): Option[Time] = {
    for {
      hours <- Try((json \ "hours").as[Int])
      minutes <- Try((json \ "minutes").as[Int]).recover { case _ => 0 }
    } yield Time(hours, minutes)
  }.toOption
}

case class Time(hours: Int = 0, minutes: Int = 0) extends Ordered[Time] {

  require(hours >= 0 && hours <= 23, "The hours must be within 0 and 23")
  require(minutes >= 0 && minutes <= 59, "The minutes must be within 0 and 23")

  val asMinutes: Int = hours * 60 + minutes

  def minus(that: Time): Int = asMinutes - that.asMinutes

  def -(that: Time): Int = minus(that)

  override lazy val toString: String = f"$hours%02d:$minutes%02d"

  override def compare(that: Time): Int = asMinutes - that.asMinutes

  def toJson: JsValue = Json.obj("hours" -> hours, "minutes" -> minutes)
}