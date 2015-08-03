package withoutbreeze

import math.exp
import scala.collection.immutable.List
import scala.annotation.tailrec
import withoutbreeze.MnistDataset
import withoutbreeze.Mnist

object Start {

  /*
   * Starten des Programms
   */
  def main(args: Array[String]) {

    val train_Set = Mnist.trainDataset;
    val test_set = Mnist.testDataset;

    val test_network = new Network(List(train_Set.imagesAsVectors.iterator.next().size, 30, train_Set.labelsAsVectors.iterator.next().size))
    val testInput = List(1.0, 1.0)

    print("created " + test_network.sizes.size + " layer network with ")
    test_network.sizes.foreach((x: Int) => print(x + " "))
    println("neurons")

    println("biases are: " + test_network.biases)
    println("weights are: " + test_network.weights)

    //    println("---------------------------------------------------------------")
    //    println("output is: " + test_network.feedforwardSigmoid(dataSet.imagesAsVectors.iterator.next()))
    //    println("output should be: " + dataSet.labelsAsVectors.iterator.next())
    println("---------------------------------------------------------------")
    println("start learning")

    test_network.SGD(train_Set, 30, 10, 0.01, test_set)

  }

}

class Network(s: List[Int]) {

  val numOfImages = 1000
  val numOfTestImages = 1000
  val sizes = s

  require(!sizes.isEmpty)

  //create random biases for the neurons starting at the second layer
  //  val biases = (sizes drop 1) map (s => randomList(s))
  var biases = createBiases(randomList)

  //create random weights for the neurons
  //  val weights = ((sizes take sizes.size - 1) zip (sizes drop 1)) map ((s: (Int, Int)) => twoDimRandomList(s))
  var weights = createWeights(twoDimRandomList)

  def createWeights[A](f: ((Int, Int)) => List[List[A]]): List[List[List[A]]] = {
    return ((sizes take sizes.size - 1) zip (sizes drop 1)) map ((s: (Int, Int)) => f(s))
  }

  def createBiases[A](f: (Int) => List[A]): List[List[A]] = {
    return (sizes drop 1) map (s => f(s))
  }

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

  def filledListOfSizeIt[A](it: Iterator[A])(s: Int): List[A] = {

    @tailrec
    def loop(i: Int, acc: List[A]): List[A] =
      if (i == 0) acc
      else loop(i - 1, it.next() :: acc)
    loop(s, List[A]())

  }

  def filledTwoDimListOfSize[A](f: () => A)(x: (Int, Int)): List[List[A]] = {

    @tailrec
    def loop(a: Int, acc: List[List[A]]): List[List[A]] =
      if (a == 0) acc
      else loop(a - 1, filledListOfSize(f)(x._1) :: acc)

    loop(x._2, List[List[A]]())

  }

  def filledTwoDimListOfSizeIt[A](i: Iterator[A])(x: (Int, Int)): List[List[A]] = {

    @tailrec
    def loop(a: Int, acc: List[List[A]]): List[List[A]] =
      if (a == 0) acc
      else loop(a - 1, filledListOfSizeIt(i)(x._1) :: acc)

    loop(x._2, List[List[A]]())

  }

  /**
   * Return the output of the network.
   */
  def feedforward(f: Double => Double)(input: List[Double], w: List[List[List[Double]]], b: List[List[Double]]): List[Double] = {

    //loop the layers
    @tailrec
    def loopL(i: Int, accL: List[Double]): List[Double] = {

      //loop the neurons
      @tailrec
      def loopN(j: Int, l: Int, li: List[Double], accN: List[Double]): List[Double] = {
        if (j == w(l - 1).size) accN
        else loopN(j + 1, l, li, accN :+ f((li zip w(l - 1)(j)).map { Function.tupled(_ * _) }.sum + b(l - 1)(j)))
      }

      if (i == sizes.size) accL
      else loopL(i + 1, loopN(0, i, accL, List[Double]()))

    }
    if (sizes.head != input.size) throw new IllegalAccessException("input layer and input values must be of same size! input: " + input.size + " layer: " + sizes.head)
    loopL(1, input)

  }

  def evaluate(test_data: MnistDataset): Long = {
    def loop(xs: Stream[(List[Int], Int)], right: Long): Long = {
      if (xs.isEmpty) return right else return loop(xs.tail, if (feedforwardSigmoid(xs.head._1.map { x => x.toDouble }, weights, biases).zipWithIndex.maxBy(_._1)._2 == xs.head._2) right + 1 else right)
    }
    return loop(test_data.examples.take(numOfTestImages), 0)
  }

  def SGD(training_data: MnistDataset, epochs: Int, mini_batch_size: Int, eta: Double, test_data: MnistDataset) {
    val d = training_data.examplesVectors.take(numOfImages)

    for (a <- 1 to epochs) {
      loop(d, 0)
      println("epoch " + a + " " + evaluate(test_data) + "/" + numOfImages)
    }

    def loop(data: Stream[(List[Int], List[Double])], count: Long) {
      if (data.isEmpty)
        return
      else
        update_mini_batch(data.take(mini_batch_size), mini_batch_size, eta)
      loop(data.drop(mini_batch_size), count + mini_batch_size)
    }
  }

  def update_mini_batch(mini_batch: Stream[(List[Int], List[Double])], mini_batch_size: Int, eta: Double) {
    def doUpdate(t: ((List[Int], List[Double]))) {
      val (delta_nabla_w, delta_nabla_b) = backprop(t._1, t._2)

      val wNewFlatten = ((delta_nabla_w.flatten.flatten) zip (weights.flatten.flatten)) map (t => (t._2 - (eta / mini_batch_size) * (t._1)))
      val bNewFlatten = (delta_nabla_b.flatten zip biases.flatten) map (t => (t._2 - (eta / mini_batch_size) * t._1))
      weights = createWeights(filledTwoDimListOfSizeIt(wNewFlatten.iterator))
      biases = createBiases(filledListOfSizeIt(bNewFlatten.iterator))

    }
    mini_batch.foreach(doUpdate)

  }

  def backprop(x: List[Int], y: List[Double]): Tuple2[List[List[List[Double]]], List[List[Double]]] = {

    var activation = x.map { x => x.toDouble }
    var activations = List(activation)
    var zs = List[List[Double]]()
    
    var nabla_b = List[List[Double]]()
    var nabla_w = List[List[List[Double]]]()
    
    weights.zip(biases).foreach(x => addValues(activation, List(x._1), List(x._2)))

    def addValues(a: List[Double], w: List[List[List[Double]]], b: List[List[Double]]) {
      val z = feedforwardId(activation, w, b)
      zs = zs.:+(z)
      activation = vectorize(sigmoid, z)
      activations = activations.:+(activation)
    }

    var delta = cost_derivative(activations.last, y)
    nabla_b = delta :: nabla_b
//    val d = dotw(delta,activations.reverse.tail.head)
    val d = delta.map ( x => activations.reverse.tail.head.map { y => x*y })
    nabla_w = d :: nabla_w
    
    def helper(nb: List[List[Double]], nw: List[List[List[Double]]], ac: List[List[Double]], zs_lokal: List[List[Double]], de: List[List[Double]], w: List[List[List[Double]]]):(List[List[List[Double]]], List[List[Double]])={
      if(ac.isEmpty){
        return (nw, nb)
      }else{
        val z = zs_lokal.head
        val spv = vectorize(sigmoid_prime, z)
//            delta = dotw(w.head, delta) * spv
//            nabla_w[-l] = np.dot(delta, activations[-l-1].transpose())
        helper(delta :: nabla_b, nabla_w,activations.tail,zs_lokal.tail, de, w)
      }
    }
    helper(nabla_b, nabla_w,activations.reverse, zs.reverse, d, weights)
  }
  
  def cost_derivative(output_activations: List[Double], y: List[Double]):List[Double]={
    ???
  }

  def feedforwardId = feedforward((x: Double) => x)(_, _, _)
  def feedforwardSigmoid = feedforward(sigmoid)(_, _, _)

  // Miscellaneous functions
  def sigmoid(z: Double): Double = 1.0 / (1.0 + exp(-z))
  def sigmoid_prime(z: Double):Double= sigmoid(z)*(1-sigmoid(z))
  
  def vectorize(f:Double => Double, v: List[Double]):List[Double]={
    v map { x => f(x) } 
  }
  
  def dotw(a:List[Double], b:List[Double]):List[List[Double]]={
    a.map ( x => b.map { y => x*y })
  }

}

