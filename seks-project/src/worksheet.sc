
object worksheet {
  val network = new Network(List(1, 2, 3))        //> network  : Network = Network@7d261f7f

  val r = scala.util.Random                       //> r  : util.Random.type = scala.util.Random$@4ff561a7
  r.nextGaussian()                                //> res0: Double = -0.8301971702464904


  var l = List(1, 2, 3)                           //> l  : List[Int] = List(1, 2, 3)
  def make_random[A](i: Int, f: () => A): List[A] = {
    def loop(a: Int, acc: List[A]): List[A] =
      if (a == 0) acc
      else loop(a - 1, f() :: acc)
    loop(i, List[A]())
  }                                               //> make_random: [A](i: Int, f: () => A)List[A]
  List[Double]() :: l.drop(1).map(x => make_random(x, scala.util.Random.nextGaussian))
                                                  //> res1: List[List[Double]] = List(List(), List(-0.9972249865530599, -2.9721853
                                                  //| 284262525), List(-0.4249097197832126, 0.35268456041213, 0.5538869799332831))
                                                  //| 
}