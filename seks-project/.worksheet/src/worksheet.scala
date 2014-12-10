
object worksheet {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(63); 
  val network = new Network(List(1, 2, 3));System.out.println("""network  : Network = """ + $show(network ));$skip(21); val res$0 = 
  network.num_layers;System.out.println("""res0: Int = """ + $show(res$0));$skip(30); 

  val r = scala.util.Random;System.out.println("""r  : util.Random.type = """ + $show(r ));$skip(19); val res$1 = 
  r.nextGaussian();System.out.println("""res1: Double = """ + $show(res$1));$skip(28); 


  var l = List(1, 2, 3);System.out.println("""l  : List[Int] = """ + $show(l ));$skip(184); 
  def make_random[A](i: Int, f: () => A): List[A] = {
    def loop(a: Int, acc: List[A]): List[A] =
      if (a == 0) acc
      else loop(a - 1, f() :: acc)
    loop(i, List[A]())
  };System.out.println("""make_random: [A](i: Int, f: () => A)List[A]""");$skip(87); val res$2 = 
  List[Double]() :: l.drop(1).map(x => make_random(x, scala.util.Random.nextGaussian));System.out.println("""res2: List[List[Double]] = """ + $show(res$2))}
}
