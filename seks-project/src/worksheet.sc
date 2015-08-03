import java.util.zip.GZIPInputStream
import java.io.FileInputStream
import java.io.BufferedInputStream

object worksheet {


  def dot(activation: List[Int], delta: List[Int]): List[List[Int]] = {
    delta.map ( x => activation.map { y => x*y })
  }                                               //> dot: (activation: List[Int], delta: List[Int])List[List[Int]]
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

  val activation = List(1, 2, 3)                  //> activation  : List[Int] = List(1, 2, 3)
  val delta = List(3, 4)                          //> delta  : List[Int] = List(3, 4)
//activation.map((delta,_).zipped.map(_*_).sum)
	val l = delta.map ( x => activation.map { y => x * y })
                                                  //> l  : List[List[Int]] = List(List(3, 6, 9), List(4, 8, 12))
  
  dot (activation, delta)                         //> res0: List[List[Int]] = List(List(3, 6, 9), List(4, 8, 12))
val de = List(List(1,2,3)).transpose.map((List(2,2),_).zipped.map(_*_).sum)
                                                  //> de  : List[Int] = List(2, 4, 6)


}