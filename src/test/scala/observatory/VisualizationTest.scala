package observatory
import observatory.Visualization._

import org.junit.Assert._
import org.junit.Test
import scala.math.{Pi, round}

trait VisualizationTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("raw data display", 2) _

  // Implement tests for the methods of the `Visualization` object

  @Test def `Check that distance of same point is 0`: Unit = {
    val first = Location(50.321, 20.112)
    val second = Location(50.321, 20.112)
    assert(greatCircleDistance(first, second) == 0, "Same point should have distance 0")
  }

  @Test def `Check that distance of opposite point is half Circumference`: Unit = {
    val first = Location(50.321, 20.112)
    val second = Location(-50.321, -159.888)
    val expectedDistance = Pi*RADIUS
    val distance = greatCircleDistance(first, second)
    assert(expectedDistance == distance, s"Opposite point should have distance $expectedDistance but have $distance")
  }


  @Test def `Check nontrivial point`: Unit = {
    val first = Location(50.00123, 47.3321)
    val second = Location(-12.10012, 20.0022)
    val expectedDist = 7401
    assert(round(greatCircleDistance(first, second)) == expectedDist, s"Given points should have distance $expectedDist")
  }



  @Test def `Check average temperature on list of 1`: Unit = {
    val temp: Temperature = 18.57 
    val loc: Location = Location(50.00123, 47.3321)
    val targetLoc: Location = Location(89.00123, 77.3321)
    val input = List((loc, temp))
    val predictedTemp = predictTemperature(input, targetLoc)
    assertEquals(temp, predictedTemp, 0.0001)
  }



}
