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

  /**
   * Identify the k centroids of a dataset
   *
   * @param dist       distance function
   * @param factory    provider of the initial centroids
   * @param aggregator function to choose the centroid based on the current centroid and the new data
   * @param dataSet    the data to be clustered
   * @param k          amount of clusters
   * @return a tuple of the final centroids and a classifier
   */
  def run[A](factory: CentroidsFactory[A])(aggregator: ToCentroid[A])(dataSet: List[A])(k: Int)(implicit dist: Distance[A]): (List[A], Classifier[A]) = {
    //  update Centroids and the assignment of observations to Centroids
    def update(centroids: List[A])(assignment: Option[List[(Int, Double)]]): (List[A], List[(Int, Double)]) = {
      val samplesAndClosestCentroid = dataSet
        .map(obs => closestCentroid(centroids)(obs))
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

  private[kmeans] def anyAssignmentChanged[A](assignment: Option[List[(Int, Double)]], nextAssignment: List[(Int, Double)]): Boolean = {
    assignment match {
      case Some(previousAssignment) =>
        previousAssignment
          .zip(nextAssignment)
          .exists { case ((i, _), (j, _)) => !(i == j) }
      case None =>
        true // initially we have no assignment
    }
  }

  /*
  * Update each Centroid position:
  * - extract cluster of points assigned to each Centroid
  * - and compute the new Centroid by aggregating cluster
  */
  private[kmeans] def recalculateCentroids[A](dataSet: List[A])(centroids: List[A])(centroidAssignedAndDistance: List[(Int, Double)])(aggregator: ToCentroid[A]): List[A] = {
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

  /**
   * Returns the
   * - index of centroid
   * - distance to the centroid
   * which is closest to observation
   */
  def closestCentroid[A](centroids: List[A])(obs: A)(implicit dist: Distance[A]): (Int, Double) = {
    centroids
      .zipWithIndex
      .map { case (value, i) => (i, dist(value)(obs)) }
      .minBy { case (i, value) => value }
  }

  def randomCentroids[A](sample: Seq[A])(k: Int) =
    scala.util.Random
      .shuffle(sample)
      .take(k)

  /**
   * Recompute Centroid as average of given sample. if no samples, keep current centroid
   */
  def avgCentroid(currentCentroid: List[Double])(samples: List[List[Double]]): List[Double] = {
    def sumPoints(v1: List[Double], v2: List[Double]): List[Double] =
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
