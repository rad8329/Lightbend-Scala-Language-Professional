package com.lightbend.training.scalatrain

sealed abstract class TrainInfo {
  def number: Int
}

case class InterCityExpress(override val number: Int, hasWifi: Boolean = false) extends TrainInfo

case class RegionalExpress(number: Int) extends TrainInfo

case class BavarianRegional(override val number: Int) extends TrainInfo

case class Station(name: String)

case class Train(info: TrainInfo, schedule: Seq[(Time, Station)]) {
  require(schedule.size >= 2, "The schedule must contain at least 2 elements")

  val stations: Seq[Station] = schedule.map(_._2)

  def timeAt(anStation: Station): Option[Time] = {
    schedule.find {
      case (_, station: Station) => station == anStation
    }.map {
      case (time, _) => time
    }
  }
}
