import math.exp
import scala.collection.immutable.List
import scala.annotation.tailrec

object Start {

  /*
   * Starten des Programms
   */
  def main(args: Array[String]) {
    val test_network = new Network(List(2, 10, 5, 2, 1))
    val testInput = List(1.0, 1.0)

    print("created " + test_network.sizes.size + " layer network with ")
    test_network.sizes.foreach((x: Int) => print(x + " "))
    println("neurons")

    println("biases are: " + test_network.biases)
    println("weights are: " + test_network.weights)

    //println("output is: " + test_network.feedforwardId(List(1, 1)))
    println("---------------------------------------------------------------")

    print("testing the network with ")
    testInput.foreach((x: Double) => print(x + " "))
    println("as input")
    println("output is: " + test_network.feedforwardSigmoid(testInput))

  }

}

class Network(s: List[Int]) {

  val sizes = s

  require(!sizes.isEmpty)

  //create random biases for the neurons starting at the second layer
  val biases = (sizes drop 1) map (s => randomList(s))

  //create random weights for the neurons
  val weights = ((sizes take sizes.size - 1) zip (sizes drop 1)) map ((s: (Int, Int)) => twoDimRandomList(s))

  def randomList = filledListOfSize(random)(_)
  def twoDimRandomList = filledTwoDimListOfSize(random)(_)

  def random() = scala.util.Random.nextGaussian
  //  def random() = { 1 }

  /**
   * creates a List with random elements for a given number
   */
  def filledListOfSize[A](f: () => A)(s: Int): List[A] = {

    @tailrec
    def loop(i: Int, acc: List[A]): List[A] =
      if (i == 0) acc
      else loop(i - 1, f() :: acc)
    loop(s, List[A]())

  }

  def filledTwoDimListOfSize[A](f: () => A)(x: (Int, Int)): List[List[A]] = {

    @tailrec
    def loop(a: Int, acc: List[List[A]]): List[List[A]] =
      if (a == 0) acc
      else loop(a - 1, filledListOfSize(f)(x._1) :: acc)

    loop(x._2, List[List[A]]())

  }

  /**
   * Return the output of the network.
   */
  def feedforward(f: Double => Double)(input: List[Double]): List[Double] = {

    //loop the layers
    @tailrec
    def loopL(i: Int, accL: List[Double]): List[Double] = {

      //loop the neurons
      @tailrec
      def loopN(j: Int, l: Int, li: List[Double], accN: List[Double]): List[Double] = {
        if (j == weights(l - 1).size) accN
        else loopN(j + 1, l, li, accN :+ f((li zip weights(l - 1)(j)).map { Function.tupled(_ * _) }.sum + biases(l - 1)(j)))
      }

      if (i == sizes.size) accL
      else loopL(i + 1, loopN(0, i, accL, List[Double]()))

    }
    if (sizes.head != input.size) throw new IllegalAccessException("input layer and input values must be of same size!")
    loopL(1, input)

  }

  def feedforwardId = feedforward((x: Double) => x)(_)
  def feedforwardSigmoid = feedforward(sigmoid)(_)

  // Miscellaneous functions
  def sigmoid(z: Double): Double = 1.0 / (1.0 + exp(-z))

}

