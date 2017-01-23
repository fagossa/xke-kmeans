package fr.xebia.streams

import fr.xebia.kmeans.Centroids.{ avgCentroid, randomCentroids }
import fr.xebia.kmeans.distance.Distances.Geometric.euclidean
import fr.xebia.kmeans.{ Centroids, CentroidsFactory, KMeans }
import org.scalatest._

class KMeansSpec extends FunSpec with MustMatchers {

  describe("Distance") {

    it("should calculate euclidean among dimensions") {
      val x: Seq[Double] = List(-1, 2, 3)
      val y: Seq[Double] = List(4, 0, -3)
      val distance: Double = euclidean(x)(y)
      distance mustBe Math.sqrt(65)
    }

  }

  describe("Kmeans") {

    it("should detect the closest centroid") {
      def assertCentroidEquals(index: Int, centroids: List[Seq[Double]], samples: List[Seq[Double]]): Any = {
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
      val k: Int = 2

      // When
      //val factory: CentroidsFactory[Double] = dataSet => k => randomCentroids(dataSet)(k)
      val factory: CentroidsFactory[Seq[Double]] = dataSet => k => dataSet(0) :: dataSet(3) :: Nil

      val (identifiedCentroids, classifier) =
        KMeans.run[Seq[Double]](euclidean)(factory)(avgCentroid)(data)(k)

      // Then
      info(s"Centroids identified $identifiedCentroids")

      val expectedCentroids = List(
        List(1.25, 1.5),
        List(3.9, 5.1)
      )
      identifiedCentroids mustBe expectedCentroids
    }
  }

}
