

object worksheet {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(67); 
  val network = new Network(Vector(1, 2, 3));System.out.println("""network  : Network = """ + $show(network ));$skip(30); 

  val r = scala.util.Random;System.out.println("""r  : util.Random.type = """ + $show(r ));$skip(19); val res$0 = 
  r.nextGaussian();System.out.println("""res0: Double = """ + $show(res$0));$skip(26); 

  var l = List(1, 2, 3);System.out.println("""l  : List[Int] = """ + $show(l ));$skip(184); 
  def make_random[A](i: Int, f: () => A): List[A] = {
    def loop(a: Int, acc: List[A]): List[A] =
      if (a == 0) acc
      else loop(a - 1, f() :: acc)
    loop(i, List[A]())
  };System.out.println("""make_random: [A](i: Int, f: () => A)List[A]""");$skip(87); val res$1 = 
  List[Double]() :: l.drop(1).map(x => make_random(x, scala.util.Random.nextGaussian));System.out.println("""res1: List[List[Double]] = """ + $show(res$1));$skip(43); 


 
  val inputs = Vector[Double](1,3,2);System.out.println("""inputs  : scala.collection.immutable.Vector[Double] = """ + $show(inputs ));$skip(89); 
  val weights = Vector[Vector[Double]]((Vector[Double](1,1,1)),(Vector[Double](1,1,2)) );System.out.println("""weights  : scala.collection.immutable.Vector[Vector[Double]] = """ + $show(weights ));$skip(59); val res$2 = 
  (inputs zip weights(0)).map{ Function.tupled(_ * _)}.sum;System.out.println("""res2: Double = """ + $show(res$2))}
  
  
}
