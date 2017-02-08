package fr.xebia.streams

import fr.xebia.kmeans.Centroids.avgCentroid
import fr.xebia.kmeans.distance.DistanceLike
import fr.xebia.kmeans.{ Centroids, CentroidsFactory, Distance, KMeans }
import org.scalatest._

class KMeansSpec extends FunSpec with MustMatchers {

  describe("Distance") {

    it("should calculate euclidean among dimensions") {
      val x: List[Double] = List(-1, 2, 3)
      val y: List[Double] = List(4, 0, -3)
      val distance: Double = DistanceLike.DistanceLikeDoubles.distance(x)(y)
      distance mustBe Math.sqrt(65)
    }

  }

  describe("Kmeans") {

    it("should detect the closest centroid") {
      def assertCentroidEquals(index: Int, centroids: List[List[Double]], samples: List[List[Double]])(implicit dist: Distance[List[Double]]): Any = {
        samples
          .map(obs => Centroids.closestCentroid(centroids)(obs))
          .map { case (centroidIndex, distance) => centroidIndex }
          .distinct.head mustBe index
      }

      implicit def dist = DistanceLike.DistanceLikeDoubles.distance _
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

    it("should cluster data 'List[Double]'") {
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
      implicit def dist = DistanceLike.DistanceLikeDoubles.distance _
      //val factory: CentroidsFactory[Double] = dataSet => k => randomCentroids(dataSet)(k)
      val factory: CentroidsFactory[List[Double]] = dataSet => k => dataSet(0) :: dataSet(3) :: Nil

      val (identifiedCentroids, classifier) =
        KMeans.run[List[Double]](factory)(avgCentroid)(data)(k)

      // Then
      info(s"Centroids identified $identifiedCentroids")

      val expectedCentroids = List(
        List(1.25, 1.5),
        List(3.9, 5.1)
      )
      identifiedCentroids mustBe expectedCentroids
    }

    it("should cluster data 'String'") {
      import DistanceLike._
      def distance[T](x: T)(y: T)(implicit dist: DistanceLike[T]) = {
        dist.distance(x)(y)
      }

      val toRoot = "word"
      val wordList = List("mundo", "world", "monde")
        .sortWith((a, b) => distance(toRoot)(a) < distance(toRoot)(b))
      wordList mustBe List("world", "monde", "mundo")
    }
  }

}
