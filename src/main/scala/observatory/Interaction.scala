package observatory
import Visualization._

import scala.math._
import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction extends InteractionInterface {

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
     new Location(
        toDegrees(atan(sinh(Pi * (1.0 - 2.0 * tile.y.toDouble / (1<<tile.zoom))))), 
        tile.x.toDouble / (1<<tile.zoom) * 360.0 - 180.0)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    val img = Image(256, 256) 
    for(x <- 0 until 256;
        y <- 0 until 256){
      val loc = tileLocation(Tile(tile.x+x, tile.y+y, tile.zoom+8))
      val temp = predictTemperature(temperatures, loc)
      val col = interpolateColor(colors, temp)
      val pix = Pixel(col.red, col.green, col.blue, 1)
      img.setPixel(x, y, pix)
      }
    img
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit = {
      for(pair <- yearlyData;
          zoom <- 0 to 3;
          x <- 0 until pow(2,zoom).toInt;
          y <- 0 until pow(2,zoom).toInt){
              val data = pair._2
              val year = pair._1
              generateImage(year, Tile(x,y,zoom), data)
          } 
  }

}
