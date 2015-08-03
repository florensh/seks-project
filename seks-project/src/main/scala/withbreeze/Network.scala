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
  
  var deltas = List[DenseMatrix[Double]]()
  var MES_values = List[Double]()

  var biases = (sizes drop 1) map (s => DenseMatrix.rand(s, 1))
  var weights = ((sizes take sizes.size - 1) zip (sizes drop 1)) map (s => DenseMatrix.rand(s._2, s._1))

  def feedforward(a: DenseMatrix[Double]): DenseMatrix[Double] = {
    def loop(w: List[DenseMatrix[Double]], b: List[DenseMatrix[Double]], acc: DenseMatrix[Double]): DenseMatrix[Double] = {
      if (w.isEmpty) acc else loop(w.tail, b.tail, sigmoid((w.head * acc) + b.head))
    }
    loop(weights, biases, a)
  }

  def evaluate(test_data: MnistDataset): Long = {
    var results = List[Tuple2[Int, Int]]()
    test_data.examples.take(numOfTestImages).foreach(t => results = results.:+((argmax(feedforward(t._1))._1, t._2)))
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

    def loop(data: Stream[(DenseMatrix[Double], DenseMatrix[Double])], count: Long) {
      if (data.isEmpty) return else update_mini_batch(data.take(mini_batch_size), mini_batch_size, eta)
      loop(data.drop(mini_batch_size), count + mini_batch_size)
    }
  }

  def update_mini_batch(mini_batch: Stream[(DenseMatrix[Double], DenseMatrix[Double])], mini_batch_size: Int, eta: Double) {

    var nabla_b = (sizes drop 1) map (s => DenseMatrix.zeros[Double](s, 1))
    var nabla_w = ((sizes take sizes.size - 1) zip (sizes drop 1)) map (s => DenseMatrix.zeros[Double](s._2, s._1))
    def doUpdate(t: ((DenseMatrix[Double], DenseMatrix[Double]))) {
      val (delta_nabla_w, delta_nabla_b) = backprop(t._1, t._2)
      nabla_b = (nabla_b zip delta_nabla_b).map(s => s._1 + s._2)
      nabla_w = (nabla_w zip delta_nabla_w).map(s => s._1 + s._2)
    }
    
    mini_batch.foreach(doUpdate)
    weights = (weights zip nabla_w).map(s => s._1-(eta/mini_batch_size)*s._2)
    biases = (biases zip nabla_b).map(s => s._1-(eta/mini_batch_size)*s._2)
    
    

  }

  def backprop(x: DenseMatrix[Double], y: DenseMatrix[Double]): Tuple2[List[DenseMatrix[Double]], List[DenseMatrix[Double]]] = {
    var activation = x
    var activations = List(activation)
    var zs = List[DenseMatrix[Double]]()

    weights.zip(biases).foreach(x => addValues(activation, x._1, x._2))

    def addValues(a: DenseMatrix[Double], w: DenseMatrix[Double], b: DenseMatrix[Double]) {
      val z = (w * a) + b
      zs = zs.:+(z)
      activation = sigmoid(z)
      activations = activations.:+(activation)
    }

//    val delta = cost_derivative(activations.reverse.head, y)
    val delta = cost.delta(zs.reverse.head, activations.reverse.head, y)
    deltas = deltas.:+(delta)
    val nabla_b = delta :: List[DenseMatrix[Double]]()
    val nabla_w = (activations.reverse.tail.head * delta.t).t :: List[DenseMatrix[Double]]()

    def helper(nb: List[DenseMatrix[Double]], nw: List[DenseMatrix[Double]], ac: List[DenseMatrix[Double]], zs_lokal: List[DenseMatrix[Double]], de: DenseMatrix[Double], w: List[DenseMatrix[Double]]): Tuple2[List[DenseMatrix[Double]], List[DenseMatrix[Double]]] = {
      if (nb.size == biases.size) {
        return (nw, nb)
      } else {
        val z = zs_lokal.head
        val spv = sigmoidPrime(z)
        val delta = (w.head.t * de)
        helper(delta :: nb, (ac.tail.head * delta.t).t :: nw, ac.tail, zs_lokal.tail, delta, w.tail)
      }
    }
    helper(nabla_b, nabla_w, activations.reverse.tail, zs.reverse, delta, weights.reverse)
  }

  def cost_derivative(output_activations: DenseMatrix[Double], y: DenseMatrix[Double]): DenseMatrix[Double] = {
    (output_activations - y)
  }
  
  def computeCost(deltas: List[DenseMatrix[Double]]):Double={
    val v = (deltas.map(x=> x :* x).reduceLeft{(acc, n) => acc + n})
    sum(v)/ (2 * deltas.size) 
  }
  
  
      def total_cost(data: Stream[(DenseMatrix[Double], DenseMatrix[Double])], lmbda:Double):Double={
        var c = 0.0
        data.foreach(x => doWork(x))
        def doWork(d: (DenseMatrix[Double], DenseMatrix[Double])){
          val a = feedforward(d._1)
          c = c + cost.fn(d._1, d._2)/numOfImages
          
        }
        
        // TODO
        
//        cost += 0.5*(lmbda/len(data))*sum(
//            np.linalg.norm(w)**2 for w in self.weights)
        return c
      }
  
  //activations
  def sigmoid(x: DenseMatrix[Double]): DenseMatrix[Double] = x.map(sigmoid(_))
  def sigmoid(input: Double): Double = 1.0 / (1.0 + Math.exp(-input))
  def sigmoidPrime(x: DenseMatrix[Double]): DenseMatrix[Double] = x.map(sigmoidPrime(_))
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
  def sigmoid(x: DenseMatrix[Double]): DenseMatrix[Double] = x.map(sigmoid(_))
  def sigmoid(input: Double): Double = 1.0 / (1.0 + Math.exp(-input))
  def sigmoidPrime(x: DenseMatrix[Double]): DenseMatrix[Double] = x.map(sigmoidPrime(_))
  def sigmoidPrime(input: Double): Double = sigmoid(input) * (1.0 - sigmoid(input))
  
  def fn(a: DenseMatrix[Double], y: DenseMatrix[Double]): Double = {
    val v = norm(a.toDenseVector - y.toDenseVector)
    v * v * 0.5

  }

  def delta(z: DenseMatrix[Double], a: DenseMatrix[Double], y: DenseMatrix[Double]): DenseMatrix[Double] = {
    (a - y) :* sigmoid(z)
  }

}

object CrossEntropyCost{
    def fn(a: DenseMatrix[Double], y: DenseMatrix[Double]): Double = {
    val v = norm(a.toDenseVector - y.toDenseVector)
    v * v * 0.5

  }

  def delta(z: DenseMatrix[Double], a: DenseMatrix[Double], y: DenseMatrix[Double]): DenseMatrix[Double] = {
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

