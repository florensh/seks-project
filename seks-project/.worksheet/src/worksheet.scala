import java.util.zip.GZIPInputStream
import java.io.FileInputStream
import java.io.BufferedInputStream

object worksheet {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(253); 


  def dot(activation: List[Int], delta: List[Int]): List[List[Int]] = {
    delta.map ( x => activation.map { y => x*y })
  };System.out.println("""dot: (activation: List[Int], delta: List[Int])List[List[Int]]""");$skip(371); 
/*
  def dot2(a: List[Int], b: List[Int]): List[List[Int]] = {
    val retVal = List[List[Int]]()

    def loop1(acc: List[List[Int]], i: Int, j: Int): List[List[Int]] = {
      if (i == 0) {
        return acc
      } else {
      	val l = List()
        loop1(acc.:+l, i - 1, j)
      }
    }
    loop1(retVal, a.size, b.size)
  }
*/

  val activation = List(1, 2, 3);System.out.println("""activation  : List[Int] = """ + $show(activation ));$skip(25); 
  val delta = List(3, 4);System.out.println("""delta  : List[Int] = """ + $show(delta ));$skip(105); 
//activation.map((delta,_).zipped.map(_*_).sum)
	val l = delta.map ( x => activation.map { y => x * y });System.out.println("""l  : List[List[Int]] = """ + $show(l ));$skip(29); val res$0 = 
  
  dot (activation, delta);System.out.println("""res0: List[List[Int]] = """ + $show(res$0));$skip(76); 
val de = List(List(1,2,3)).transpose.map((List(2,2),_).zipped.map(_*_).sum);System.out.println("""de  : List[Int] = """ + $show(de ))}


}
