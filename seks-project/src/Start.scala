
object Start {

  /*
   * Starten des Programms
   */
  def main(args: Array[String]) {
    println("Working...")
    println(new Network(List(784, 30, 10)).biases)
    println(new Network(List(784, 30, 10)).weights)
    println("Finished!")

  }

}
/**
 * The list ``sizes`` contains the number of neurons in the
 * respective layers of the network.  For example, if the list
 * was [2, 3, 1] then it would be a three-layer network, with the
 * first layer containing 2 neurons, the second layer 3 neurons,
 * and the third layer 1 neuron.
 */
class Network(sizes: List[Int]) {

  /**
   * creates a List with random elements for a given number
   */
  def randomList[A](i: Int, f: () => A): List[A] = {
    def loop(a: Int, acc: List[A]): List[A] =
      if (a == 0) acc
      else loop(a - 1, f() :: acc)
    loop(i, List[A]())
  }

  //create random biases for the neurons starting at the second layer
  var biases = List[Double]() :: sizes.drop(1).map(x => randomList(x, scala.util.Random.nextGaussian))

  //create random weights for the neurons
  var weights = sizes.take(2).zip(sizes.drop(1)).map((x: (Int, Int)) => randomList(x._1 * x._2, scala.util.Random.nextGaussian))

}