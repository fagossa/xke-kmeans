package fr.xebia

package object kmeans {

  type Classifier[A] = A => A

  type Distance[A] = A => A => Double

  type CentroidsFactory[A] = Seq[A] => Int => Seq[A]

  type ToCentroid[A] = A => Seq[A] => A

}
