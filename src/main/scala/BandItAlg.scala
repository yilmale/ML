import java.io.{File, PrintWriter}

import breeze.linalg._
import breeze.stats.distributions.{Gaussian, Rand, Uniform}

/**
  * Created by yilmaz on 6/12/17.
  */
object BanditAlg {
  def bandit(epsilon : Double): Unit = {
    val NS : Int = 10
    val ITER : Int = 999
    var count : Int = 0
    val gau = new Gaussian(0,1)
    val r = scala.util.Random
    val writer = new PrintWriter(new File("test.txt" ))

    var Q = DenseVector.zeros[Double](NS)
    var Qs = DenseVector.tabulate[Double](NS){i => gau.sample()}
    var N = DenseVector.zeros[Int](NS)


    def getReward(A : Int): Double = {
      val R : Double = gau.sample() + Qs(A)
      R
    }


    println("Action values: ")
    for (i <- 0 until NS) print(Qs(i) + " ")

    var A : Int = 0
    var cumReward : Double = 0
    var avgReward : Array[Double] = new Array[Double](1000)
    var curReward : Double = 0
    val ud = Uniform(0,1)
    val ui = Rand.randInt(0,10)
    do {
      if (ud.draw() <= epsilon)
        A =ui.draw()
      else A = argmax(Q)
      N(A) += 1
      curReward = getReward(A)
      cumReward += curReward
      Q(A) += (1/N(A)) * (curReward - Q(A))
      count += 1
      avgReward(count) = cumReward / count
      writer.println("step: " + count + " average reward: " + avgReward(count))
    } while (count < ITER)

    writer.close()
    println()
    println("Estimated values: ")
    for (i <- 0 until NS) print(Q(i) + " ")

  }
}