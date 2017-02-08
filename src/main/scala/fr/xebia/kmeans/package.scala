package fr.xebia

package object kmeans {

  type Classifier[A] = A => A

  type Distance[A] = A => A => Double

  type CentroidsFactory[A] = List[A] => Int => List[A]

  type ToCentroid[A] = A => List[A] => A

}
