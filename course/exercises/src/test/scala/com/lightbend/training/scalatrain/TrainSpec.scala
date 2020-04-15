/*
 * Copyright © 2012 - 2016 Lightbend, Inc. All rights reserved.
 */

package com.lightbend.training.scalatrain

import TestData._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TrainSpec extends AnyWordSpec with Matchers {

  "Train ice724" should {
    "stop in Nurember" in {
      ice724.timeAt(nuremberg) shouldEqual Some(ice724NurembergTime)
    }
    "not stop in Essen" in {
      ice724.timeAt(essen) shouldEqual None
    }
  }

  "Train ice726" should {
    "stop in Munich" in {
      ice726.timeAt(munich) shouldEqual Some(ice726MunichTime)
    }
    "not stop in Cologne" in {
      ice726.timeAt(cologne) shouldEqual None
    }
  }

  "Creating a Train" should {
    "throw an IllegalArgumentException for a schedule with 0 or 1 elements" in {
      an[IllegalArgumentException] should be thrownBy Train(InterCityExpress(724), Vector())
      an[IllegalArgumentException] should be thrownBy Train(InterCityExpress(724), Vector(ice724MunichTime -> munich))
    }
  }

  "stations" should {
    "be initialized correctly" in {
      ice724.stations shouldEqual Vector(munich, nuremberg, frankfurt, cologne)
    }
  }
}
