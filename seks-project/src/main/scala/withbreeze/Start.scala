package withbreeze
import breeze.linalg._
import Array._
import withbreeze.org.scalann.examples.Mnist
import breeze.linalg._
import breeze.plot._
import breeze.numerics._
import java.awt.{Color, Paint}
/**
 * @author Studium
 */
object Start {
  def main(args: Array[String]): Unit = {


    var conf = new NetworkConfiguration()
      .addNumTestImages(10000)
      .addNumImages(10000)
      .addNumHiddenLayers(List(400))
      .addDataSet(Mnist.trainDataset)
      .addTestDataSet(Mnist.testDataset)
//      .addCostLogFunction { x => println("cost " + x) }

    val net = new Network(conf)
    net.SGD(30, 10, 0.5)

    val f2 = Figure()
    f2.width_=(900)
    f2.height_=(900)
    
    var n = 0
    Mnist.trainDataset.vector_matrix_labels.drop(151).take(25).foreach(d => printResults(d))
    def printResults(test1: ((DenseVector[Double], DenseMatrix[Int]), Int)) {
      val test1X = test1._1._1
      val test1Y = test1._2
      val test1Erg = net.feedforward(test1X)

      val mat = DenseMatrix.tabulate(28, 28) { case (i, j) => test1._1._2(-i, j) }

      println("sollte sein: ")
      println(test1Y)
      println("ist: ")
      println(test1Erg)

      val img = mat.map(xi => xi.toDouble)
      val sp = f2.subplot(5, 5, n) += image(img)
      sp.title = "found value is " + test1Erg.argmax.toString
      sp.logScaleX_=(false)
      f2.refresh
      n = n + 1

    }
    


  }

}
