package observatory

import observatory.Extraction._
import java.time.LocalDate
import org.junit.Assert._
import org.junit.{Test, Ignore}
import scala.io.Source

trait ExtractionTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("data extraction", 1) _

  // Implement tests for the methods of the `Extraction` object

  val stationsFile = "/stations.csv"
  val temperaturesFile = "/temperatures.csv"
  val precision = 0.001

  @Test def `Check that test resources exist and are not empty`: Unit = {
    val stationsSource = Source.fromURL(getClass.getResource(stationsFile))
    val temperaturesSource = Source.fromURL(getClass.getResource(stationsFile))
    assert(!stationsSource.isEmpty, s"$stationsFile should should not be empty")
    assert(!temperaturesSource.isEmpty, s"$temperaturesFile should should not be empty")
  }


  @Test def `Check reading in temperatures as ds`: Unit = {
    val ds = temperaturesDS(2015, temperaturesFile)
    assert(ds.count()==4)
  }


  @Test def `Check reading in stations as ds`: Unit = {
    val ds = stationsDS(stationsFile)
    assert(ds.count()==2)
  }


  @Test def `Check that locate Temperatures works`: Unit = {
    val expected = Seq(
      (LocalDate.of(2015, 8, 11), Location(37.35, -78.433), 27.3),
      (LocalDate.of(2015, 12, 6), Location(37.358, -78.438), 0.0),
      (LocalDate.of(2015, 1, 29), Location(37.358, -78.438), 2.0)
    )
    val actual = locateTemperatures(2015, stationsFile, temperaturesFile).toSeq
    assert((0 until expected.size).forall((i: Int) => (expected(i)._1 == actual(i)._1) 
      & (expected(i)._2 == actual(i)._2)
      & (expected(i)._3 - actual(i)._3).abs < precision))
  }


  @Test def `Check that average Temperatures works`: Unit = {
    val expected = Seq(
      (Location(37.35, -78.433), 27.3),
      (Location(37.358, -78.438), 1.0)
    )
    val temps = locateTemperatures(2015, stationsFile, temperaturesFile).toSeq
    val actual = locationYearlyAverageRecords(temps).toSeq
    assert((0 until expected.size).forall((i: Int) => (expected(i)._1 == actual(i)._1) 
      & (expected(i)._2 - actual(i)._2).abs < precision))
  }


}
