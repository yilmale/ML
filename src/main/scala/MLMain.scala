import BanditAlg._
import breeze.linalg._
import breeze.optimize.DiffFunction
import breeze.stats.distributions._
import breeze.plot._
import breeze.optimize._
import xcs.XCSAlgorithm.model
import xcs._

import org.nlogo.app.App
import java.awt.EventQueue
import org.nlogo.headless.HeadlessWorkspace



object MLMain {
  def main(args: Array[String]): Unit = {
    val workspace = HeadlessWorkspace.newInstance
    workspace.open("/Users/yilmaz/Desktop/NetLogo 6.0.3/models/Sample Models/Earth Science/Fire.nlogo")
    workspace.command("setup")
    workspace.command("go")
    println(workspace.report("burned-trees"))
    workspace.dispose()
  }

  /*
  App.app.command("set density 62")
  App.app.command("random-seed 0")
  App.app.command("setup")
  App.app.command("repeat 50 [ go ]")
  println(App.app.report("burned-trees"))*/


  /*
  var s = new MUXProblem(10000)
  XCSAlgorithm(s)

  println("Final population in sorted order of fitness")
  var rules = model.sort()
  rules foreach (r => print(r))

*/
  /*
 var epsilon : Double = 0.1
 bandit(epsilon)
 DpIPE()
 println("Starting version with policy improvement")
 DpPI()
 MCPE()
 TempDif()
 QLearning()
 def f(xs: DenseVector[Double])  = {
   sum(xs ^:^ 2.0)
 }
 def gradf(xs: DenseVector[Double]) = {
   xs *:* 2.0
 }
 val optTrait = new DiffFunction[DenseVector[Double]] {
   def calculate(xs: DenseVector[Double]) = (f(xs),gradf(xs))
 }
 val xs1 = DenseVector.ones[Double](3)
 println("Starting to print f")
 println(f(xs1))
 println("Starting to print grad")
 println(gradf(xs1))
 val min = minimize(optTrait,DenseVector(1.0,1.0,1.0))
 println("Minimum is at " + min)
 def g(x: DenseVector[Double]) = sum((x - 3.0) ^:^ 2.0)
 val gv = g(DenseVector(0.0,0.0,0.0))
 val d = new ApproximateGradientFunction[Int,DenseVector[Double]](g)
 println("gradient is " + d.gradientAt(DenseVector(1.0,2.0,3.0)))
 val min1 = minimize(d,DenseVector(1.0,1.0,1.0))
 println("Minimum is at " + min1)
 val a = DenseVector(10.0,20.0,30.0)
 val b = DenseVector(1.0, 2.0, 3.0)
 val c = a +:+ b
 println("the result is " + c)
 println("the sum is " + sum(c))
 val n= 5
 val dm = DenseMatrix.zeros[Double](n,n)
 println(dm)
 println("row 0 is ")
 println(dm(0, ::))
 println("column 0 is ")
 println(dm(::,0))
 dm(::,2) := 5.0
 println(dm)
 println(dm.t)
 val dm1 = DenseMatrix.ones[Double](5,3)
 println(dm1)
 var dm2= dm * dm1
 println(dm2)
 val dm3 = a dot b
 println(dm3)
 val maxA = max(a)
 val maxArg = argmax(a)
 println((maxA,maxArg))
*/

  /*
  val x = DenseVector.zeros[Double](5)
  x(1) = 2
  println(x)
  val m = DenseMatrix.zeros[Int](5,5)
  println(m)
  println((m.rows, m.cols))
  m(4,::) := DenseVector(1,2,3,4,5).t
  println(m)
  val poi = new Gaussian(0,1)
  val s = poi.sample(5)
  println(s)
  val f = Figure()*/
  /*val p = f.subplot(0)
  val x1 = linspace(0.0,1.0)
  p += plot(x1, x1 :^ 2.0)
  p += plot(x1, x1 :^ 3.0, '.')
  p.xlabel = "x axis"
  p.ylabel = "y axis"
  f.saveas("lines.png")
  val p2 = f.subplot(2,1,1)
  val g = breeze.stats.distributions.Gaussian(0,1)
  p2 += hist(g.sample(100000),100)
  p2.title = "A normal distribution"
  f.saveas("subplots.png")*/


}