package fr.xebia.kmeans.distance

trait DistanceLike[T] {
  def distance(x: T)(y: T): Double
}
object DistanceLike {
  implicit object DistanceLikeDoubles extends DistanceLike[List[Double]] {
    def distance(x: List[Double])(y: List[Double]): Double = {
      val sum = x.zip(y)
        .map { case (pi, qi) => Math.pow(qi - pi, 2) }
        .sum
      Math.sqrt(sum)
    }
  }

  implicit object DistanceLikeString extends DistanceLike[String] {
    import scala.math._
    def minimum(i1: Int, i2: Int, i3: Int) = min(min(i1, i2), i3)
    def distance(x: String)(y: String): Double = {
      val dist = Array.tabulate(
        y.length + 1, x.length + 1
      ) { (j, i) =>
          if (j == 0) i else if (i == 0) j else 0
        }
      for (j <- 1 to y.length; i <- 1 to x.length)
        dist(j)(i) =
          if (y(j - 1) == x(i - 1)) dist(j - 1)(i - 1)
          else minimum(
            dist(j - 1)(i) + 1,
            dist(j)(i - 1) + 1,
            dist(j - 1)(i - 1) + 1
          )
      dist(y.length)(x.length)
    }
  }
}
