package withbreeze
import breeze.linalg._
import breeze.linalg.operators
import breeze.numerics._
import withbreeze.org.scalann.examples.Mnist
import withbreeze.org.scalann.examples.MnistDataset
import math.exp
import withbreeze.org.scalann.examples.MnistDataset
import breeze.stats.mean
import breeze.numerics._
import breeze.math._
import breeze.optimize._

/**
 * @author Studium
 */
class Network(config: NetworkConfiguration) {
  val sizes = config.sizes
  val numOfImages = config.numOfImages
  val numOfTestImages = config.numOfTestImages
  val testDataSet = config.testDataSet
  val trainDataSet = config.dataSet
  val plotfunction = config.plotfunction
  val costLogFunction = config.costLogFunction
  val cost = CrossEntropyCost
  
  var deltas = List[DenseVector[Double]]()
  var MES_values = List[Double]()

  var biases = (sizes drop 1) map (s => DenseVector.rand(s))
  var weights = ((sizes take sizes.size - 1) zip (sizes drop 1)) map (s => DenseMatrix.rand(s._2, s._1))

  def feedforward(a: DenseVector[Double]): DenseVector[Double] = {
    def loop(w: List[DenseMatrix[Double]], b: List[DenseVector[Double]], acc: DenseVector[Double]): DenseVector[Double] = {
      if (w.isEmpty) acc else loop(w.tail, b.tail, sigmoid((w.head * acc) + b.head))
    }
    loop(weights, biases, a)
  }

  def evaluate(test_data: MnistDataset): Long = {
    var results = List[Tuple2[Int, Int]]()
    test_data.examples.take(numOfTestImages).foreach(t => results = results.:+((argmax(feedforward(t._1)), t._2)))
    return results.filter(e => e._1 == e._2).size
  }

  def SGD(epochs: Int, mini_batch_size: Int, eta: Double) {
    val lmbda = 0.0
    println("Start training")
    val data = trainDataSet.examplesVectors.take(numOfImages)

    for (a <- 1 to epochs) {
      loop(data, 0)
      println("epoch " + a + " " + evaluate(testDataSet) + "/" + numOfTestImages + " cost " + computeCost(deltas))
    }

    def loop(data: Stream[(DenseVector[Double], DenseVector[Double])], count: Long) {
      if (data.isEmpty) return else update_mini_batch(data.take(mini_batch_size), mini_batch_size, eta, lmbda)
      loop(data.drop(mini_batch_size), count + mini_batch_size)
    }
  }

  def update_mini_batch(mini_batch: Stream[(DenseVector[Double], DenseVector[Double])], mini_batch_size: Int, eta: Double, lmbda: Double) {

    var nabla_b = (sizes drop 1) map (s => DenseVector.zeros[Double](s))
    var nabla_w = ((sizes take sizes.size - 1) zip (sizes drop 1)) map (s => DenseMatrix.zeros[Double](s._2, s._1))
    def doUpdate(t: (DenseVector[Double], DenseVector[Double])) {
      val (delta_nabla_w, delta_nabla_b) = backprop(t._1, t._2)
      nabla_b = (nabla_b zip delta_nabla_b).map(s => s._1 + s._2)
      nabla_w = (nabla_w zip delta_nabla_w).map(s => s._1 + s._2)
    }

    mini_batch.foreach(doUpdate)
//    weights = (weights zip nabla_w).map(s => s._1 - (eta / mini_batch_size) * s._2)
//    biases = (biases zip nabla_b).map(s => s._1 - (eta / mini_batch_size) * s._2)
    weights = (weights zip nabla_w).map(s => (1-eta*(lmbda/numOfImages)) * s._1 - (eta/mini_batch_size) * s._2)
    biases = (biases zip nabla_b).map(s => s._2-(eta/mini_batch_size) * s._2)
    
    

  }

  def backprop(x: DenseVector[Double], y: DenseVector[Double]): Tuple2[List[DenseMatrix[Double]], List[DenseVector[Double]]] = {
    var activation = x
    var activations = List(activation)
    var zs = List[DenseVector[Double]]()

    weights.zip(biases).foreach(x => feedforward(activation, x._1, x._2))

    def feedforward(a: DenseVector[Double], w: DenseMatrix[Double], b: DenseVector[Double]) {
      val z = (w * a) + b
      zs = zs :+ z
      activation = sigmoid(z)
      activations = activations :+ activation
    }

    val delta = cost.delta(zs.reverse.head, activations.reverse.head, y)
    deltas = deltas :+ delta
    val nabla_b = delta :: List[DenseVector[Double]]()
    val nabla_w = (activations.reverse.tail.head * delta.t).t :: List[DenseMatrix[Double]]()

    def helper(nb: List[DenseVector[Double]], nw: List[DenseMatrix[Double]], ac: List[DenseVector[Double]], zs_lokal: List[DenseVector[Double]], de: DenseVector[Double], w: List[DenseMatrix[Double]]): Tuple2[List[DenseMatrix[Double]], List[DenseVector[Double]]] = {
      if (nb.size == biases.size) {
        return (nw, nb)
      } else {
        val z = zs_lokal.head
        val spv = sigmoidPrime(z)
        // TODO: müsste eigentlich noch mal spv sein. sp ergibt aber immer 0.0
        val delta = (w.head.t * de)
        helper(delta :: nb, (ac.tail.head * delta.t).t :: nw, ac.tail, zs_lokal.tail, delta, w.tail)
      }
    }
    helper(nabla_b, nabla_w, activations.reverse.tail, zs.reverse.tail, delta, weights.reverse)
  }

  def cost_derivative(output_activations: DenseMatrix[Double], y: DenseMatrix[Double]): DenseMatrix[Double] = {
    (output_activations - y)
  }
  
  def computeCost(deltas: List[DenseVector[Double]]):Double={
    val v = (deltas.map(x=> x :* x).reduceLeft{(acc, n) => acc + n})
    sum(v)/ (2 * deltas.size) 
  }
  
  
      def total_cost(data: Stream[(DenseVector[Double], DenseVector[Double])], lmbda:Double):Double={
        var c = 0.0
        data.foreach(x => doWork(x))
        def doWork(d: (DenseVector[Double], DenseVector[Double])){
          val a = feedforward(d._1)
          c = c + cost.fn(d._1, d._2)/numOfImages
          
        }
        
        // TODO
        
//        cost += 0.5*(lmbda/len(data))*sum(
//            np.linalg.norm(w)**2 for w in self.weights)
        return c
      }
  
  //activations
  def sigmoid(x: DenseVector[Double]): DenseVector[Double] = x.map(sigmoid(_))
  def sigmoid(input: Double): Double = 1.0 / (1.0 + Math.exp(-input))
  def sigmoidPrime(x: DenseVector[Double]): DenseVector[Double] = x.map(sigmoidPrime(_))
  def sigmoidPrime(input: Double): Double = sigmoid(input) * (1.0 - sigmoid(input))
  def tanh(x: DenseVector[Double]): DenseVector[Double] = x.map(tanh(_))
  def tanh(input: Double): Double = Math.tanh(input)
  def tanhPrime(x: DenseVector[Double]): DenseVector[Double] = x.map(tanhPrime(_))
  def tanhPrime(input: Double): Double = 1.0 - Math.pow(tanh(input), 2.0)
  def softmax(x: DenseVector[Double]): DenseVector[Double] = {
    val denom = sum(x.map(Math.exp))
    x.map(Math.exp(_) / denom)
  }

}

object QuadraticCost {
  def sigmoid(x: DenseVector[Double]): DenseVector[Double] = x.map(sigmoid(_))
  def sigmoid(input: Double): Double = 1.0 / (1.0 + Math.exp(-input))
  def sigmoidPrime(x: DenseVector[Double]): DenseVector[Double] = x.map(sigmoidPrime(_))
  def sigmoidPrime(input: Double): Double = sigmoid(input) * (1.0 - sigmoid(input))
  
  def fn(a: DenseVector[Double], y: DenseVector[Double]): Double = {
    val v = norm(a.toDenseVector - y.toDenseVector)
    v * v * 0.5

  }

  def delta(z: DenseVector[Double], a: DenseVector[Double], y: DenseVector[Double]): DenseVector[Double] = {
    (a - y) :* sigmoidPrime(z)
  }

}

object CrossEntropyCost{
    def fn(a: DenseVector[Double], y: DenseVector[Double]): Double = {
    val v = norm(a.toDenseVector - y.toDenseVector)
    v * v * 0.5

  }

  def delta(z: DenseVector[Double], a: DenseVector[Double], y: DenseVector[Double]): DenseVector[Double] = {
    (a-y)
  }
  
}

class NetworkConfiguration {

  lazy val sizes = dataSet.imagesAsVectors.iterator.next().size :: numHiddenLayers.:+(dataSet.labelsAsVectors.iterator.next().size)
  var numOfTestImages = 0
  var numOfImages = 0
  var numHiddenLayers = List[Int]()
  var dataSet: MnistDataset = null
  var testDataSet: MnistDataset = null
  var plotfunction:List[Double] => Unit = (x=>Unit)
  var costLogFunction:Double => Unit = (x=>Unit)

  def addNumTestImages(n: Int): NetworkConfiguration = {
    numOfTestImages = n
    this
  }
  
  def addPlotfunction(f: List[Double] => Unit): NetworkConfiguration = {
    plotfunction = f
    this
  }
  
  def addCostLogFunction(f: Double => Unit): NetworkConfiguration = {
    costLogFunction = f
    this
  }

  def addNumImages(n: Int): NetworkConfiguration = {
    numOfImages = n
    this
  }

  def addNumHiddenLayers(hl: List[Int]): NetworkConfiguration = {
    numHiddenLayers = hl
    this
  }

  def addDataSet(ds: MnistDataset): NetworkConfiguration = {
    dataSet = ds
    this
  }

  def addTestDataSet(ds: MnistDataset): NetworkConfiguration = {
    testDataSet = ds
    this
  }

}

