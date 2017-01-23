package fr.xebia.kmeans

import fr.xebia.kmeans.Centroids.closestCentroid

/*
 * Algorithm:
 * - Starting from k random Centroids,
 * - Observations are assigned to the closest Centroid, and constitute a Cluster,
 * - Centroids are updated, by taking the average of their Cluster,
 * - Until the allocation of Observation to Clusters doesn’t change any more.
 */
object KMeans {

  // Given a distance, centroid factory and
  // centroid aggregation function, identify
  // the k centroids of a dataset
  def run[A](dist: Distance[A])(factory: CentroidsFactory[A])(aggregator: ToCentroid[A])(dataSet: Seq[A])(k: Int): (Seq[A], Classifier[A]) = {
    // Recursively update Centroids and
    // the assignment of observations to Centroids
    def update(centroids: Seq[A])(assignment: Option[Seq[(Int, Double)]]): (Seq[A], Seq[(Int, Double)]) = {
      val samplesAndClosestCentroid = dataSet
        .map(obs => closestCentroid(dist)(centroids)(obs))
      val changed = anyAssignmentChanged(assignment, samplesAndClosestCentroid)
      if (changed) {
        val updatedCentroids = recalculateCentroids(dataSet)(centroids)(samplesAndClosestCentroid)(aggregator)
        update(updatedCentroids)(Some(samplesAndClosestCentroid)) // another round of updates
      } else {
        (centroids, samplesAndClosestCentroid)
      }
    }
    val initialCentroids = factory(dataSet)(k)
    val centroids = update(initialCentroids)(None)._1
    val classifier = (dataPoint: A) => centroids.minBy(centroid => dist(centroid)(dataPoint))
    (centroids, classifier)
  }

  private[kmeans] def anyAssignmentChanged[A](assignment: Option[Seq[(Int, Double)]], nextAssignment: Seq[(Int, Double)]): Boolean = {
    assignment match {
      case Some(previousAssignment) =>
        previousAssignment
          .zip(nextAssignment)
          .exists { case ((i, _), (j, _)) => !(i == j) }
      case None =>
        true // initially we have no assignment
    }
  }

  // Update each Centroid position:
  // extract cluster of points assigned to each Centroid
  // and compute the new Centroid by aggregating cluster
  private[kmeans] def recalculateCentroids[A](dataSet: Seq[A])(centroids: Seq[A])(centroidAssignedAndDistance: Seq[(Int, Double)])(aggregator: ToCentroid[A]): Seq[A] = {
    val assignedDataSet = dataSet.zip(centroidAssignedAndDistance)
    centroids
      .zipWithIndex
      .map {
        case (centroid, i) =>
          val samplesAssignedToCentroid = assignedDataSet
            .filter { case ((_, (ci, _))) => ci == i }
            .map { case (obs, _) => obs }
          aggregator(centroid)(samplesAssignedToCentroid)
      }
  }

}

object Centroids {

  // Returns the
  // - index of centroid
  // - distance to the centroid
  // which is closest to observation
  def closestCentroid[A](dist: Distance[A])(centroids: Seq[A])(obs: A): (Int, Double) = {
    centroids
      .zipWithIndex
      .map { case (value, i) => (i, dist(value)(obs)) }
      .minBy { case (i, value) => value }
  }

  def randomCentroids[A](sample: Seq[A])(k: Int) =
    scala.util.Random
      .shuffle(sample)
      .take(k)

  // Recompute Centroid as average of given sample
  def avgCentroid(currentCentroid: Seq[Double])(samples: Seq[Seq[Double]]): Seq[Double] = {
    def sumPoints(v1: Seq[Double], v2: Seq[Double]): Seq[Double] =
      v1.zip(v2).map { case (v1x, v2x) => v1x + v2x }

    val size = samples.length
    size match {
      case 0 => currentCentroid
      case _ =>
        samples
          .reduce(sumPoints)
          .map(e => e / size)
    }
  }
}
