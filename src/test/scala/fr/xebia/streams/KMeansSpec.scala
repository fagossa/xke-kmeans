package fr.xebia.streams

import fr.xebia.kmeans.distance.GeometricDistances
import fr.xebia.kmeans.{CentroidsFactory, Classifier, KMeans, Sample}
import org.scalatest._

import scala.util.Random

class KMeansSpec extends FlatSpec with MustMatchers {

  "Euclidean distance" should "be calculated among dimensions" in {
    val x: Sample[Double] = List(-1, 2, 3)
    val y: Sample[Double] = List(4, 0, -3)
    val distance: Double = GeometricDistances.euclidean(x)(y)
    distance mustBe Math.sqrt(65)
  }

  "" should "" in {

    val centroids = List(
      List(0.0, 0.0, 0.0),
      List(20.0, 30.0, 40.0),
      List(-40.0, -50.0, -60.0)
    )

    val data = centroids.map { centroid =>
      (1 to 50).map(x => x + 5 * Random.nextDouble() - 0.5)
    }

    val factory: CentroidsFactory[Double] = ??? //KMeans.randomCentroids()

    val identifiedCentroids, classifier = KMeans[Double](GeometricDistances.euclidean)(factory)(KMeans.avgCentroid)(data)

    println(s"Centroids identified $identifiedCentroids")
  }

}
