package fr.xebia.streams

import fr.xebia.kmeans.Sample
import fr.xebia.kmeans.distance.GeometricDistances
import org.scalatest._

class KMeansSpec extends FlatSpec with MustMatchers {

  "Euclidean distance" should "be calculated among dimensions" in {
    val x: Sample[Double] = List(-1, 2, 3)
    val y: Sample[Double] = List(4, 0, -3)
    val distance: Double = GeometricDistances.euclidean(x, y)
    distance mustBe Math.sqrt(65)
  }

}
