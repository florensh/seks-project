package withoutbreeze

import java.util.zip.GZIPInputStream
import java.io.FileInputStream
import java.io.BufferedInputStream
import java.io.DataInputStream
import java.nio.file.Paths
import scala.Stream

class MnistFileReader(location: String, fileName: String) {
  private[this] val path = Paths.get(location, fileName)
  protected[this] val stream = new DataInputStream(new GZIPInputStream(new BufferedInputStream(new FileInputStream(path.toString))))

}

class MnistLabelReader(location: String, fileName: String) extends MnistFileReader(location, fileName) {

  assert(stream.readInt() == 2049, "Wrong MNIST label stream magic")

  val count = stream.readInt()

  val labelsAsInts = readLabels(0)
  val labelsAsVectors = labelsAsInts.map { label =>
    List.tabulate[Double](10) { i => if (i == label) 1.0 else 0.0 }
  }

  private[this] def readLabels(ind: Int): Stream[Int] =
    if (ind >= count)
      Stream.empty
    else
      Stream.cons(stream.readByte(), readLabels(ind + 1))

}

class MnistImageReader(location: String, fileName: String) extends MnistFileReader(location, fileName) {

  assert(stream.readInt() == 2051, "Wrong MNIST image stream magic")

  val count = stream.readInt()
  val width = stream.readInt()
  val height = stream.readInt()

  val imagesAsVectors = readImages(0)

  private[this] def readImages(ind: Int): Stream[List[Int]] =
    if (ind >= count)
      Stream.empty
    else
      Stream.cons(readImage(), readImages(ind + 1))

  private[this] def readImage(): List[Int] = {

    def loop(i: Int, acc: List[Int]): List[Int] = {
      if (i == 0) {
        return acc
      } else {
        return loop(i - 1, acc :+ (stream.readUnsignedByte()))
      }
    }
    loop(height * width, List())
  }

}
class MnistDataset(location: String, dataset: String) {

  lazy val imageReader = new MnistImageReader(location, s"$dataset-images-idx3-ubyte.gz")
  lazy val labelReader = new MnistLabelReader(location, s"$dataset-labels-idx1-ubyte.gz")

  def imageWidth = imageReader.width
  def imageHeight = imageReader.height

  def imagesAsVectors = imageReader.imagesAsVectors

  def labelsAsInts = labelReader.labelsAsInts
  def labelsAsVectors = labelReader.labelsAsVectors

  def examples = imagesAsVectors zip labelsAsInts
  def examplesVectors = imagesAsVectors zip labelsAsVectors

}

object Mnist {

  val location = "E:\\privat\\Studium\\SEKS\\project\\git\\seks-project\\data"

  val trainDataset = new MnistDataset(location, "train")
  val testDataset = new MnistDataset(location, "t10k")
  

}