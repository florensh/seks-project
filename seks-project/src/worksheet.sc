

object worksheet {
  val network = new Network(Vector(1, 2, 3))      //> network  : Network = Network@2dbae104

  val r = scala.util.Random                       //> r  : util.Random.type = scala.util.Random$@62dba79e
  r.nextGaussian()                                //> res0: Double = 0.2938310221727854

  var l = List(1, 2, 3)                           //> l  : List[Int] = List(1, 2, 3)
  def make_random[A](i: Int, f: () => A): List[A] = {
    def loop(a: Int, acc: List[A]): List[A] =
      if (a == 0) acc
      else loop(a - 1, f() :: acc)
    loop(i, List[A]())
  }                                               //> make_random: [A](i: Int, f: () => A)List[A]
  List[Double]() :: l.drop(1).map(x => make_random(x, scala.util.Random.nextGaussian))
                                                  //> res1: List[List[Double]] = List(List(), List(-0.1553072565304044, -0.7303029
                                                  //| 414333448), List(1.6419923597761763, -0.1157356548360616, 1.040471503712809)
                                                  //| )


 
  val inputs = Vector[Double](1,3,2)              //> inputs  : scala.collection.immutable.Vector[Double] = Vector(1.0, 3.0, 2.0)
                                                  //| 
  val weights = Vector[Vector[Double]]((Vector[Double](1,1,1)),(Vector[Double](1,1,2)) )
                                                  //> weights  : scala.collection.immutable.Vector[Vector[Double]] = Vector(Vector
                                                  //| (1.0, 1.0, 1.0), Vector(1.0, 1.0, 2.0))
  (inputs zip weights(0)).map{ Function.tupled(_ * _)}.sum
                                                  //> res2: Double = 6.0
  
  
}