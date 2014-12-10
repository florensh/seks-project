
object worksheet {
  val network = new Network(List(1, 2, 3))        //> network  : Network = Network@4deb033c
  network.num_layers                              //> res0: Int = 3

  val r = scala.util.Random                       //> r  : util.Random.type = scala.util.Random$@6ba00355
  r.nextGaussian()                                //> res1: Double = 1.4257945787212616


  var l = List(1, 2, 3)                           //> l  : List[Int] = List(1, 2, 3)
  def make_random[A](i: Int, f: () => A): List[A] = {
    def loop(a: Int, acc: List[A]): List[A] =
      if (a == 0) acc
      else loop(a - 1, f() :: acc)
    loop(i, List[A]())
  }                                               //> make_random: [A](i: Int, f: () => A)List[A]
  List[Double]() :: l.drop(1).map(x => make_random(x, scala.util.Random.nextGaussian))
                                                  //> res2: List[List[Double]] = List(List(), List(-0.2852827734999528, 0.34573942
                                                  //| 23865781), List(-0.9264802956999006, -0.5973723365880447, 1.8112903223091974
                                                  //| ))
}