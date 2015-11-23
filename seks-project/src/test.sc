import breeze.linalg._
import breeze.linalg.operators
import breeze.numerics._
object test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val m = DenseMatrix.ones[Int](5,5)              //> m  : breeze.linalg.DenseMatrix[Int] = 1  1  1  1  1  
                                                  //| 1  1  1  1  1  
                                                  //| 1  1  1  1  1  
                                                  //| 1  1  1  1  1  
                                                  //| 1  1  1  1  1  
  m(4,::) := DenseVector(1,2,3,4,5).t             //> res0: breeze.linalg.Transpose[breeze.linalg.DenseVector[Int]] = Transpose(De
                                                  //| nseVector(1, 2, 3, 4, 5))
  m                                               //> res1: breeze.linalg.DenseMatrix[Int] = 1  1  1  1  1  
                                                  //| 1  1  1  1  1  
                                                  //| 1  1  1  1  1  
                                                  //| 1  1  1  1  1  
                                                  //| 1  2  3  4  5  
  val a = m(4,::)                                 //> a  : breeze.linalg.Transpose[breeze.linalg.DenseVector[Int]] = Transpose(Den
                                                  //| seVector(1, 2, 3, 4, 5))
  
  a dot a                                         //> res2: Int = 55
  
  val x = DenseVector(1.0,2,0)                    //> x  : breeze.linalg.DenseVector[Double] = DenseVector(1.0, 2.0, 0.0)
  val w = DenseMatrix((1.0,1.0),(1.0,1.0),(1.0,1.0))
                                                  //> w  : breeze.linalg.DenseMatrix[Double] = 1.0  1.0  
                                                  //| 1.0  1.0  
                                                  //| 1.0  1.0  
  val b = DenseVector(1.0,1.0,1.0)                //> b  : breeze.linalg.DenseVector[Double] = DenseVector(1.0, 1.0, 1.0)
  
 val dv = DenseVector.ones[Int](2)                //> dv  : breeze.linalg.DenseVector[Int] = DenseVector(1, 1)
 val dm = DenseMatrix.ones[Int](3,2)              //> dm  : breeze.linalg.DenseMatrix[Int] = 1  1  
                                                  //| 1  1  
                                                  //| 1  1  
 val y = dm * dv                                  //> y  : <error> = DenseVector(2, 2, 2)
  
}