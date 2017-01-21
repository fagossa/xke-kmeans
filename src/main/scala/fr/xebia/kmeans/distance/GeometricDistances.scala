package fr.xebia.kmeans.distance

import fr.xebia.kmeans._

object GeometricDistances {

  def euclidean(x: Sample[Double], y: Sample[Double]): Double = {
    val sum = x.zip(y)
      .map { case (pi, qi) => Math.pow(qi - pi, 2) }
      .sum
    Math.sqrt(sum)
  }

}
