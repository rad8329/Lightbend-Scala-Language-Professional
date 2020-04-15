package com.lightbend.training.scalatrain

class JourneyPlanner(trains: Set[Train]) {
  val stations: Set[Station] = trains.flatMap(_.stations)

  def trainsAt(anStation: Station): Set[Train] = trains.filter(_.stations.contains(anStation))

  def stopsAtAlt(anStation: Station): Set[(Time, Train)] = for {
    train <- trains
    stop <- train.schedule if stop._2 == anStation
  } yield (stop._1, train)

  def stopsAt(anStation: Station): Set[(Time, Train)] = for {
    train <- trains
    time <- train.timeAt(anStation)
  } yield (time, train)

  /**
   * A trip between two stations is a short trip, if:
   *
   * - there exists a connection with a single train and
   * - there is at most one station between the given two
   */
  def isShortTripWithNoMatching(from: Station, to: Station): Boolean = {
    trains.exists(train => train.stations.dropWhile(_ != from).slice(1, 3).contains(to))
  }

  /**
   * A trip between two stations is a short trip, if:
   *
   * - there exists a connection with a single train and
   * - there is at most one station between the given two
   */
  def isShortTrip(from: Station, to: Station): Boolean = {
    trains.exists(train => train.stations.dropWhile(_ != from) match {
      case `from` +: `to` +: _ => true
      case `from` +: _ +: `to` +: _ => true
      case _ => false
    })
  }
}
