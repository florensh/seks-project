import breeze.linalg._
import breeze.linalg.operators
import breeze.numerics._
object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(136); 
  println("Welcome to the Scala worksheet");$skip(40); 
  
  val m = DenseMatrix.ones[Int](5,5);System.out.println("""m  : breeze.linalg.DenseMatrix[Int] = """ + $show(m ));$skip(38); val res$0 = 
  m(4,::) := DenseVector(1,2,3,4,5).t;System.out.println("""res0: breeze.linalg.Transpose[breeze.linalg.DenseVector[Int]] = """ + $show(res$0));$skip(4); val res$1 = 
  m;System.out.println("""res1: breeze.linalg.DenseMatrix[Int] = """ + $show(res$1));$skip(18); 
  val a = m(4,::);System.out.println("""a  : breeze.linalg.Transpose[breeze.linalg.DenseVector[Int]] = """ + $show(a ));$skip(13); val res$2 = 
  
  a dot a;System.out.println("""res2: Int = """ + $show(res$2));$skip(34); 
  
  val x = DenseVector(1.0,2,0);System.out.println("""x  : breeze.linalg.DenseVector[Double] = """ + $show(x ));$skip(53); 
  val w = DenseMatrix((1.0,1.0),(1.0,1.0),(1.0,1.0));System.out.println("""w  : breeze.linalg.DenseMatrix[Double] = """ + $show(w ));$skip(35); 
  val b = DenseVector(1.0,1.0,1.0);System.out.println("""b  : breeze.linalg.DenseVector[Double] = """ + $show(b ));$skip(38); 
  
 val dv = DenseVector.ones[Int](2);System.out.println("""dv  : breeze.linalg.DenseVector[Int] = """ + $show(dv ));$skip(37); 
 val dm = DenseMatrix.ones[Int](3,2);System.out.println("""dm  : breeze.linalg.DenseMatrix[Int] = """ + $show(dm ));$skip(17); 
 val y = dm * dv;System.out.println("""y  : <error> = """ + $show(y ))}
  
}
