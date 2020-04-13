package observatory

import org.apache.log4j.{Level, Logger}
import java.time.LocalDate
import org.apache.spark.sql._
import org.apache.spark.sql.functions._

/**
  * 1st milestone: data extraction
  */
object Extraction extends ExtractionInterface {
  
  case class Temp(SIN: Int, WBAN: Int, year: Int, month: Int, day: Int, temp: Double)

  def convertToCelcius(t: Temp): Temp = t match {case Temp(s,w,y,m,d,t) => Temp(s,w,y,m,d, (t-32)*5/9)}

  case class Station(SIN: Int, WBAN: Int, longitude: Double, latitude: Double)

  case class LocalTemp(location: Location, temperature: Temperature)

  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  val session = SparkSession.builder().appName("extraction").master("local").getOrCreate()
  import session.implicits._
  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    val tempDS: Dataset[Temp] = temperaturesDS(year, temperaturesFile)
    val statDS: Dataset[Station] = stationsDS(stationsFile)
    val joinedDS: Dataset[(Station, Temp)] = statDS.joinWith(tempDS, tempDS("SIN") === statDS("SIN") && tempDS("WBAN") === statDS("WBAN"))
    joinedDS.collect().map{case (s: Station, t: Temp) => (LocalDate.of(t.year, t.month, t.day), Location(s.longitude, s.latitude), t.temp)}
  }

  def temperaturesDS(year: Int, temperaturesFile: String): Dataset[Temp] = {
    val df = session.read.format("csv").schema("SIN integer ,WBAN integer ,month integer ,day integer, temp double")
      .load(getClass.getResource(temperaturesFile).getPath)
      .na.fill(0, Array("SIN","WBAN"))
      .withColumn("year",lit(year))
    df.as[Temp].map(convertToCelcius)
  }

  def stationsDS(stationsFile: String): Dataset[Station] = {
    val df = session.read.format("csv").schema("SIN integer ,WBAN integer ,longitude double, latitude double")
      .load(getClass.getResource(stationsFile).getPath)
      .na.fill(0, Array("SIN","WBAN"))
      .na.drop()
    df.as[Station]
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    val localTempDS: Dataset[LocalTemp] = records.map{case (_, loc: Location, temp: Temperature) => LocalTemp(loc, temp)}.toSeq.toDS()
    localTempDS.groupBy("location").agg(avg($"temperature").as("temperature")).as[LocalTemp].map((lt: LocalTemp) => (lt.location, lt.temperature)).collect()
  }

}
