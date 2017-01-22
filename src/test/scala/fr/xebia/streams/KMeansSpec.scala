package fr.xebia.streams

import fr.xebia.kmeans.Centroids.{avgCentroid, randomCentroids}
import fr.xebia.kmeans.distance.Distances.Geometric.euclidean
import fr.xebia.kmeans.{Centroids, CentroidsFactory, KMeans, Sample}
import org.scalatest._

import scala.util.Random

class KMeansSpec extends FunSpec with MustMatchers {

  describe("Distance") {

    it("should calculate euclidean among dimensions") {
      val x: Sample[Double] = List(-1, 2, 3)
      val y: Sample[Double] = List(4, 0, -3)
      val distance: Double = euclidean(x)(y)
      distance mustBe Math.sqrt(65)
    }

  }

  describe("Kmeans") {

    it("should detect the closest centroid") {
      def assertCentroidEquals(index: Int, centroids: List[List[Double]], samples: List[List[Double]]): Any = {
        samples
          .map(obs => Centroids.closestCentroid(euclidean)(centroids)(obs))
          .map { case (centroidIndex, distance) => centroidIndex }
          .distinct.head mustBe index
      }
      // Given
      val centroids = List(
        List(1.0, 1.0),
        List(5.0, 7.0)
      )
      // When / Then
      assertCentroidEquals(index = 0, centroids,
        List(List(1.0, 1.0), List(1.5, 2.0), List(3.0, 4.0)))

      assertCentroidEquals(index = 1, centroids,
        List(List(5.0, 7.0), List(3.5, 5.0), List(4.5, 5.0), List(3.5, 4.5)))
    }

    it("should cluster data") {
      // Given
      val data = List(
        List(1.0, 1.0),
        List(1.5, 2.0),
        List(3.0, 4.0),
        List(5.0, 7.0),
        List(3.5, 5.0),
        List(4.5, 5.0),
        List(3.5, 4.5)
      )
      // When
      //val factory: CentroidsFactory[Double] = dataSet => k => randomCentroids(dataSet)(k)
      val factory: CentroidsFactory[Double] = dataSet => k => dataSet(0) :: dataSet(3) :: Nil

      val (identifiedCentroids, classifier) =
        KMeans.run[Double](euclidean)(factory)(avgCentroid)(data)(2)

      // Then
      info(s"Centroids identified $identifiedCentroids")

      val expectedCentroids = List(
        List(1.25, 1.5),
        List(3.9, 5.1)
      )
      identifiedCentroids mustBe expectedCentroids
    }
  }


  ignore("should cluster data???") {
    // Given
    val centroids = List(
      List(0.0, 0.0, 0.0),
      List(20.0, 30.0, 40.0),
      List(-40.0, -50.0, -60.0)
    )

    val data = centroids.map { centroid =>
      (1 to 50).map(x => x + 5 * Random.nextDouble() - 0.5)
    }

    // When
    val factory: CentroidsFactory[Double] = dataSet => k => randomCentroids(dataSet)(k)

    val identifiedCentroids, classifier =
      KMeans.run[Double](euclidean)(factory)(avgCentroid)(data)(3)

    // Then
    println(s"Centroids identified $identifiedCentroids")

    val expectedCentroids: List[Vector[Double]] = List(
      Vector(2.8238581422873965, 3.8136218518023597, 3.778597861507187, 5.25786709277542, 6.534309738995786, 6.726665041555048, 10.304867148604336, 8.753867905566459, 9.164744374443835, 9.680795011496707, 14.659038006497214, 12.916609567332255, 12.744343922683194, 18.270456259131, 18.066837703700724, 16.777582835691213, 18.30019244609304, 22.332906142471774, 21.41834175401069, 19.533074726186857, 22.203487084537922, 25.78799337008983, 22.965685825304032, 27.136621699611197, 24.74500513293827, 27.446855653628454, 28.189383012736517, 28.21830743670509, 29.750406743523126, 32.97828721071124, 34.8552445084494, 32.96936231990658, 36.72661086099214, 37.19125389318925, 35.605200724127975, 39.86587624213138, 39.24240431057634, 40.54301095403855, 41.77785116105412, 39.62203367540127, 44.98959971416255, 44.314503173713504, 45.53836390643123, 43.561137342832616, 49.24547922023708, 50.20820647045004, 47.81238615368812, 49.67178057870646, 49.2629183206053, 52.21829289401333),
      Vector(1.9649101592656422, 3.976888856927065, 7.451505379343901, 6.498981726340089, 7.6425874323369705, 7.484945310088166, 11.081128934483605, 8.614162862734354, 10.70676276378812, 9.915569724661044, 10.638925407765864, 11.989855080046, 13.724547381584214, 15.595723487061043, 17.90114020906941, 16.61079255469533, 17.056533767274423, 19.408160396237502, 23.243369431358623, 23.00340272531118, 23.182297473713916, 26.440349232563378, 24.705815760137714, 28.32967159101541, 26.538617553850315, 28.02994186538063, 28.85460379035136, 28.979865625708264, 32.81038834402346, 34.23196576627572, 30.535865221621076, 32.39680660367259, 35.310483147022204, 38.343267915913444, 35.20621772720632, 39.99037237657758, 36.812370194548166, 38.01134009546945, 38.64142676094904, 42.908123761582864, 42.958069633981495, 44.62750919254219, 44.474451964294126, 48.11643355559335, 48.154080309671016, 48.15857386938138, 50.19474460119963, 48.71622014443977, 51.68919262205616, 53.3063227337787),
      Vector(1.9525771981734832, 4.668365153353125, 5.66064463472866, 4.900971196916231, 7.755349437153555, 8.136121937552133, 7.178866601571466, 10.732404572366182, 12.417228469728322, 11.465295415757046, 15.236097928659895, 13.602215975960744, 15.929147788322634, 15.417704906346954, 17.570450492194023, 16.23988220763631, 19.753100121442586, 20.649412950342764, 19.834530671153715, 22.868017336189986, 22.912825605592896, 21.58243434146412, 23.386521805814684, 24.624501686689552, 26.652410393056513, 28.757112402746408, 29.061667645124917, 27.753378239594145, 32.86497399994611, 33.47207937431638, 32.353386591285684, 33.461766955643434, 37.45553928655063, 37.01969171840646, 35.53578529464023, 36.60664401521414, 40.29534140407166, 41.107058891294265, 39.39313010977984, 41.92318670212833, 43.449247298837975, 46.11626083470677, 46.386872816375856, 46.33548687814266, 48.91362051962051, 49.702075548364085, 48.287116188398166, 49.000785344447195, 51.188881074549464, 49.85151481284707)
    )
    identifiedCentroids mustBe expectedCentroids
  }

}
