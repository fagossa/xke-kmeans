package fr.xebia.kmeans.distance

import fr.xebia.kmeans._

object Distances {

  object Geometric {

    val euclidean: Distance[Seq[Double]] = x => y => {
      val sum = x.zip(y)
        .map { case (pi, qi) => Math.pow(qi - pi, 2) }
        .sum
      Math.sqrt(sum)
    }

  }

  object Similarity {

    val LevenshteinDistance: Distance[String] = x => y => {
      val lenStr1 = x.length
      val lenStr2 = y.length

      val d: Array[Array[Int]] = Array.ofDim(lenStr1 + 1, lenStr2 + 1)

      for (i <- 0 to lenStr1) d(i)(0) = i
      for (j <- 0 to lenStr2) d(0)(j) = j

      for (i <- 1 to lenStr1; j <- 1 to lenStr2) {
        val cost = if (x(i - 1) == y(j - 1)) 0 else 1

        d(i)(j) = min(
          d(i - 1)(j) + 1, // deletion
          d(i)(j - 1) + 1, // insertion
          d(i - 1)(j - 1) + cost // substitution
        )
      }

      d(lenStr1)(lenStr2)
    }

    def min(nums: Int*): Int = nums.min

  }

}
