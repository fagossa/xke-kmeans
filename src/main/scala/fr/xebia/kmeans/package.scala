package fr.xebia

package object kmeans {

  type Sample[A] = Seq[A]

  type Distance[A] = Sample[A] => Sample[A] => Double

  type CentroidsFactory[A] = Seq[A] => Int => Seq[A]

  type ToCentroid[A] = Sample[A] => Seq[Sample[A]] => Sample[A]

}
