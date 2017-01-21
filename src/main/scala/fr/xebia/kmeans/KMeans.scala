package fr.xebia.kmeans

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
  def run[A](dist: Distance[A])
            (factory: CentroidsFactory[A])
            (aggregator: ToCentroid[A])
            (dataSet: Seq[Sample[A]])(k: Int) = {
    // Recursively update Centroids and
    // the assignment of observations to Centroids
    def update(centroids: Seq[Sample[A]])(assignment: Option[Seq[(Int, Double)]]): (Seq[Sample[A]], Seq[(Int, Double)]) = {
      // Assign each point to the closest centroid
      val next = dataSet
        .map(obs => closest(dist)(centroids)(obs))
      // Check if any assignment changed
      val change = assignment match {
        case Some(previous) =>
          previous.zip(next)
            .exists { case ((i, _), (j, _)) => !(i == j) }
        case None =>
          true // initially we have no assignment
      }
      if (change) {
        // Update each Centroid position:
        // extract cluster of points assigned to each Centroid
        // and compute the new Centroid by aggregating cluster
        val updatedCentroids: Seq[Sample[A]] = {
          val assignedDataSet = dataSet.zip(next)
          centroids.zipWithIndex
            .map { case (centroid, i) =>
              val samplesAssignedToCentroid = assignedDataSet
                .filter { case ((_, (ci, _))) => ci == i }
                .map { case (obs, _) => obs }
              aggregator(centroid)(samplesAssignedToCentroid)
            }
        }
        // Perform another round of updates
        update(updatedCentroids)(Some(next))
      } else {
        (centroids, next)
      }
    }

  }

  // Returns the
  // - index of centroid
  // - distance to the centroid
  // which is closest to observation
  def closest[A](dist: Distance[A])(centroids: Seq[Sample[A]])(obs: Sample[A]): (Int, Double) =
    centroids
      .zipWithIndex
      .map { case (value, i) => (i, dist(value)(obs)) }
      .minBy { case (i, value) => value }

  def randomCentroids[A](sample: Seq[A])(k: Int) =
    scala.util.Random.shuffle(sample).take(k)

  // Recompute Centroid as average of given sample
  def avgCentroid(current: Sample[Double])(sample: Seq[Sample[Double]]): Sample[Double] = {
    def sumPoints(v1: Sample[Double], v2: Sample[Double]): Sample[Double] =
      v1.zip(v2).map { case (v1x, v2x) => v1x + v2x }

    val size: Int = sample.length
    size match {
      case 0 => current
      case _ =>
        sample
          .reduce(sumPoints)
          .map(e => e / size)
    }
  }

}
