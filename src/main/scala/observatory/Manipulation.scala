package observatory
import Visualization._

/**
  * 4th milestone: value-added information
  */
object Manipulation extends ManipulationInterface {

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {
    var tempMap =  Map[GridLocation, Temperature]()
    for(lat <- -89 to 90;
        lon <- -180 to 179){
        tempMap = tempMap.updated(GridLocation(lat, lon), predictTemperature(temperatures, Location(lat.toFloat, lon.toFloat)))
        }
    tempMap
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
    val grids = temperaturess.map(makeGrid)
    def averageFun(loc: GridLocation): Temperature = {
      val temps = grids.map(_.apply(loc))
      temps.sum / temps.size
    }
    averageFun
  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {
    val grid = makeGrid(temperatures)
    def averageFun(loc: GridLocation): Temperature = {
      grid(loc) - normals(loc)
    }
    averageFun
  }


}

