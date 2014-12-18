import math.exp
import scala.collection.immutable.Vector

object Start {

  /*
   * Starten des Programms
   */
  def main(args: Array[String]) {
    val test_network = new Network(Vector(2, 10, 2))

    println(test_network.biases)
    println(test_network.weights)

    //    println(test_network.weights.zip(test_network.biases))
    println(test_network.feedforward(Vector(1, 1)))

  }

}
/**
 * The list ``sizes`` contains the number of neurons in the
 * respective layers of the network.  For example, if the list
 * was [2, 3, 1] then it would be a three-layer network, with the
 * first layer containing 2 neurons, the second layer 3 neurons,
 * and the third layer 1 neuron.
 */
class Network(sizes: Vector[Int]) {

  def random() = scala.util.Random.nextGaussian
  //  def random() = { 1 }

  /**
   * creates a List with random elements for a given number
   */
  def randomV[A](i: Int, f: () => A): Vector[A] = {
    def loop(a: Int, acc: Vector[A]): Vector[A] =
      if (a == 0) acc
      else loop(a - 1, acc.:+(f()))
    loop(i, Vector[A]())

  }

  def multi_randomV[A](i: Int, j: Int, f: () => A): Vector[Vector[A]] = {
    def loop(a: Int, acc: Vector[Vector[A]]): Vector[Vector[A]] =
      if (a == 0) acc
      else loop(a - 1, acc.:+(randomV(i, f)))
    loop(j, Vector[Vector[A]]())
  }

  //create random biases for the neurons starting at the second layer
  var biases = sizes.drop(1).map(x => randomV(x, random))

  //create random weights for the neurons
  var weights = sizes.take(2).zip(sizes.drop(1)).map((x: (Int, Int)) => multi_randomV(x._1, x._2, random))

  /**
   * Return the output of the network if ``a`` is input.
   */
  def feedforward(a: Vector[Double]): Vector[Double] = {

    //loop the layers
    def loopL(i: Int, accL: Vector[Double]): Vector[Double] = {

      //loop the neurons
      def loopN(j: Int, l: Int, li: Vector[Double], accN: Vector[Double]): Vector[Double] = {
        if (j == weights(l - 1).size) accN
        else loopN(j + 1, l, li, accN.:+(sigmoid((li zip weights(l - 1)(j)).map { Function.tupled(_ * _) }.sum + biases(l - 1)(j))))
      }

      if (i == sizes.size) accL
      else loopL(i + 1, loopN(0, i, accL, Vector[Double]()))

    }
    loopL(1, a)

  }

  // Miscellaneous functions
  def sigmoid(z: Double): Double = 1.0 / (1.0 + exp(-z))

}

