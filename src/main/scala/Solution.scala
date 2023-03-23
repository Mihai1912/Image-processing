import util.Pixel
import util.Util.{getNeighbors, readEntire, toGrayScale}

import scala.Byte.MaxValue.*

// Online viewer: https://0xc0de.fr/webppm/
object Solution {
  type Image = List[List[Pixel]]
  type GrayscaleImage = List[List[Double]]

  // prerequisites
  def fromStringPPM(image: List[Char]): Image = { // :Image
    val myImg = pars(image.toList);
    val format = "P3";
    val lungime = myImg.tail.head.mkString.toInt;
    val inaltime = myImg.tail.tail.head.mkString.toInt;
    val culoareMaxima = 255;
    val pixeli = myImg.drop(4);

    def loop(n: Int, matrice: List[List[Char]], img: Image): Image = {
      if (n > 0) {
        val linie = matrice.take(lungime * 3)

        def loopAux(m: Int, line: List[List[Char]], linePX: List[Pixel]): List[Pixel] = {
          if (m > 0) {
            val pixel = makePixel(line.take(3))
            loopAux(m - 1, line.drop(3), linePX ++ List(pixel))
          } else {
            linePX
          }
        }

        loop(n - 1, matrice.drop(lungime * 3), img ++ List(loopAux(lungime, linie, Nil)))
      } else {
        img
      }
    }

    loop(inaltime, pixeli, Nil)
  }

  def makePixel(pixeli: List[List[Char]]): Pixel = {
    val pixelValues = pixeli.map(_.mkString.toInt)
    val pixel = Pixel(pixelValues(0), pixelValues(1), pixelValues(2))
    pixel
  }

  def pars(img: List[Char]): List[List[Char]] = {
    def operation(c: Char, acc: List[List[Char]]): List[List[Char]] = {
      acc match {
        case Nil => if (c == ' ' || c == '\n') Nil else List(List(c))
        case x :: xs => if (c == ' ' || c == '\n') Nil :: acc else (c :: x) :: xs
      }
    }

    img.foldRight(Nil: List[List[Char]])(operation)
  }

  def toStringPPM(image: Image): List[Char] = {
    val ch = List("P", "3", "\n") ++ image(0).length.toString.toList ++ " " ++ image.length.toString.toList ++ "\n" ++ List(2, 5, 5) ++ "\n"
    val inaltime = image.length
    val lungime = image(0).length
    val pixeli = image.flatMap(linie => linie.flatMap(pixel => List(pixel.red.toString, " ", pixel.green.toString, " ", pixel.blue.toString, "\n")))
    (ch ++ pixeli.mkString).map(p => p.toString.head)
  }

  // ex 1
  def verticalConcat(image1: Image, image2: Image): Image = {
    image1 ++ image2
  }

  // ex 2
  def horizontalConcat(image1: Image, image2: Image): Image = {
    (image1.transpose ++ image2.transpose).transpose
  }

  // ex 3
  def rotate(image: Image, degrees: Integer): Image = {
    if (degrees == 90) {
      (image.transpose).reverse
    } else if (degrees == 180) {
      (image.map(linie => linie.reverse)).reverse
    } else if (degrees == 270) {
      image.transpose.map(linie => linie.reverse)
    } else {
      image
    }
  }

  // ex 4
  val gaussianBlurKernel: GrayscaleImage = List[List[Double]](
    List(1, 4, 7, 4, 1),
    List(4, 16, 26, 16, 4),
    List(7, 26, 41, 26, 7),
    List(4, 16, 26, 16, 4),
    List(1, 4, 7, 4, 1)
  ).map(_.map(_ / 273))

  val Gx: GrayscaleImage = List(
    List(-1, 0, 1),
    List(-2, 0, 2),
    List(-1, 0, 1)
  )

  val Gy: GrayscaleImage = List(
    List(1, 2, 1),
    List(0, 0, 0),
    List(-1, -2, -1)
  )

  def edgeDetection(image: Image, threshold: Double): Image = {
    val grayImg = image.map(row => row.map(pixel => toGrayScale(pixel)))
    val blurImag = applyConvolution(grayImg, gaussianBlurKernel)
    val Mx = applyConvolution(blurImag, Gx)
    val My = applyConvolution(blurImag, Gy)
    val x = Mx.zip(My).map(rowPair => rowPair._1.zip(rowPair._2).map(pair => pair._1.abs + pair._2.abs))
    x.map(row => row.map(elem => if (elem < threshold) Pixel(0, 0, 0) else Pixel(255, 255, 255)))
  }

  def applyConvolution(image: GrayscaleImage, kernel: GrayscaleImage): GrayscaleImage = {
    val neighbors = getNeighbors(image, (kernel.length - 1) / 2)
    val a = neighbors.map(rowMatr => rowMatr
      .map(matr => matr.zip(kernel)
        .map(rowPair => rowPair._1.zip(rowPair._2)
          .map(pair => pair._1 * pair._2).sum).sum))
    a
  }

  // ex 5
  def moduloPascal(m: Integer, funct: Integer => Pixel, size: Integer): Image = {

    def genFirstLine(n: Int, list: List[Int]): List[Int] = {
      if (n > 0) {
        genFirstLine(n - 1, list ++ List(0))
      } else {
        list
      }
    }

    val firstLine = genFirstLine(size - 1, List(1))

    def generateLine(n: Int, list: List[Int], acc: List[Int]): List[Int] = {
      if (acc.length == 0) {
        generateLine(n, list, acc ++ list.take(1))
      } else if (acc.length < n) {
        generateLine(n, list, acc ++ List((list.take(acc.length + 1).drop(acc.length - 1).sum) % m))
      } else {
        acc
      }
    }

    def generate(list: List[List[Int]], size: Int): List[List[Int]] = {
      if (list.length < size) {
        generate(list ++ List(generateLine(size, list.drop(list.length - 1).head, Nil)), size)
      } else {
        list
      }
    }

    val fin = generate(List(firstLine), size).map(row => row.map(elem => funct(elem)))


    def corectare(n: Int, size: Int, image: Image, acc: Image): Image = {
      if (n <= size) {
        val row = image.take(n).head
        corectare(n+1 , size, image.drop(1), acc ++ List(row.take(n) ++ row.drop(n).map(p => Pixel(0,0,0))))
      } else {
        acc
      }
    }

    corectare(1,size,fin,Nil)
  }
}
