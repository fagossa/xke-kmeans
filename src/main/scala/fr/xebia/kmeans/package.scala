package fr.xebia

package object kmeans {

  type Sample[A] = Seq[A]

  type Classifier[A] = Sample[A] => Sample[A]

  type Distance[A] = Sample[A] => Sample[A] => Double

  type CentroidsFactory[A] = Seq[Sample[A]] => Int => Seq[Sample[A]]

  type ToCentroid[A] = Sample[A] => Seq[Sample[A]] => Sample[A]

}
