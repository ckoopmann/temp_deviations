package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import scala.math._

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface {


  val RADIUS = 6371
  def isAntipode(first: Location, second: Location): Boolean = {
    val roundFactor = 1E4
    def roundToFactor(num: Double) = round(num*roundFactor)
    val correctLat = -second.lat 
    val correctLon = -second.lon*(180/abs(second.lon) - 1) 
    roundToFactor(first.lat) == roundToFactor(correctLat) & roundToFactor(first.lon) == roundToFactor(correctLon)
  }

  /**
    * @param first First location to measure distance from 
    * @param second Second location to get distance to
    * @return Great circle distance according to https://en.wikipedia.org/wiki/Great-circle_distance
    */
  def greatCircleDistance(first: Location, second: Location): Double = {
    val (lon1, lat1, lon2, lat2) = (toRadians(first.lon), toRadians(first.lat), toRadians(second.lon), toRadians(second.lat))

    val delta = if (first == second) 1E-10 else if (isAntipode(first, second)) Pi else
      acos(sin(lat1)*sin(lat2)+cos(lat1)*cos(lat2)*cos(lon2-lon1))

    delta*RADIUS
  }

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    val tempsWithWeight = temperatures.map{ case (loc: Location, temp: Temperature) => (1/greatCircleDistance(location, loc), temp)}.toSeq.sortBy(-_._1)
    tempsWithWeight.find(_._1 > 1) match {
      case Some((loc, temp)) => temp
      case None => {val (weighting: Double, weightedTemp: Temperature) = tempsWithWeight .foldLeft((0.0,0.0))((acc: (Double, Temperature), element: (Double, Temperature)) => (acc._1 + element._1, acc._2 + element._1*element._2))
                    weightedTemp/weighting
                   }
      }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    val lesserPoints = points.filter(_._1 <= value)
    val greaterPoints = points.filter(_._1 >= value)
    lazy val (lowerTemp, lowerCol) = lesserPoints.reduce((acc: (Temperature, Color), elem: (Temperature, Color)) => if (elem._1 > acc._1) elem else acc)
    lazy val (upperTemp, upperCol) = greaterPoints.reduce((acc: (Temperature, Color), elem: (Temperature, Color)) => if (elem._1 < acc._1) elem else acc)
    
    if(lesserPoints.isEmpty) upperCol else if(greaterPoints.isEmpty || value == lowerTemp) lowerCol else if (value == upperTemp) upperCol 
    else {
      def interpolateField(lower: Int, upper: Int): Int = round((lower*1.0 + (value - lowerTemp)*((upper - lower)/(upperTemp-lowerTemp))).toFloat)
      Color(interpolateField(lowerCol.red, upperCol.red),
        interpolateField(lowerCol.green, upperCol.green),
        interpolateField(lowerCol.blue, upperCol.blue)) 
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val img = Image(360, 180) 
    def getLocation(x: Int, y: Int) = Location(90-y, x-180)
    for(x <- 0 until 360;
        y <- 0 until 180){
      val loc = getLocation(x, y)
      val temp = predictTemperature(temperatures, loc)
      val col = interpolateColor(colors, temp)
      val pix = Pixel(col.red, col.green, col.blue, 1)
      img.setPixel(x, y, pix)
      }
   img
  }

}

