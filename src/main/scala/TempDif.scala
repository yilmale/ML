import breeze.linalg.DenseVector
import breeze.stats.distributions.Rand
import breeze.plot._
import org.jfree.chart._
import org.jfree.ui.RefineryUtilities

/**
  * Created by yilmaz on 7/24/17.
  */
object TempDif {
  var V = scala.collection.mutable.Map[String, Double]("s0" -> 0, "s1" -> 0, "s2" -> 0)
  var policy = scala.collection.mutable.Map[(String, String), Double]()
  var T = scala.collection.mutable.Map[(String, String, String), (Double, Double)]()
  val EpisodeLength : Int = 20
  val NS: Int = 3
  val S: List[String] = List("s0", "s1", "s2")
  val A: List[String] = List("a0", "a1", "a2")
  val learningRate : Double = 0.05
  val discount : Double = 0.1

  def apply(): Unit = {
    initializePolicy(policy, T)
    var episode = generateEpisode()
    println(episode)
    var s0V = DenseVector.zeros[Double](20)

    var eList = episode.iterator
    var state : String = eList.next()
    var i : Int = 0
    while (eList.hasNext) {
      var a = eList.next()
      if (eList.hasNext) {
        var st = eList.next()
        var R = getReward(state, a)
        V(state) += (learningRate * (R + (discount * V(st)) - V(state)))
        if (state == "s0") {
          s0V(i)=V(state)*100
          i += 1
        }
        state = st
      }
    }

    println("State values after TD(0):")
    for (s <- S) {
      println("State: " + s + " Value: " + V(s))
    }

    println("dense vector is " + s0V)

    /*val fig = Figure()
    val plt = fig.subplot(0)
    var xInd = DenseVector.range(0,20, 1).map( _.toDouble)
*/
    //  plt += plot(xInd,s0V)
    // fig.refresh()



  }

  def getReward(ns: String, na: String): Double = {
    var reward : Double = Rand.uniform.draw()
    reward
  }

  def generateEpisode(): List[String] = {
    var ep = scala.collection.mutable.ListBuffer[String]()
    var curState : String = "s0"

    for (i <- 0 until EpisodeLength) {
      ep += curState
      val x : Double =Rand.uniform.draw()
      var selected : String = getSelectedAction(curState)
      ep += selected
      curState  = getNextState(curState,selected)
    }

    ep.toList
  }

  def getSelectedAction(cS: String): String = {
    var s: String = ""
    var cum : Double = 0.0
    val x = Rand.uniform.draw()
    var found : Boolean = false
    for (a <- A if (found == false))  {
      if (x<=(cum+policy(cS,a))) {
        s=a
        found=true
      }
      else {
        cum = cum+policy(cS,a)
      }
    }
    s
  }

  def getNextState(cst: String, act: String): String = {
    var s: String = ""
    var cum : Double = 0.0
    val x = Rand.uniform.draw()
    var found : Boolean = false
    for (st <- S if (found==false)) {
      val p = T((cst,act,st))._1
      if (x<=(cum+p)) {
        s=st
        found=true
      }
      else {
        cum = cum+T((cst,act,st))._1
      }
    }
    s
  }

  def initializePolicy(p : scala.collection.mutable.Map[(String,String),Double],
                       T : scala.collection.mutable.Map[(String,String,String),(Double,Double)]): Unit = {
    p += (("s0", "a0") -> 0.5)
    p += (("s0", "a1") -> 0.5)
    p += (("s0", "a2") -> 0.0)

    p += (("s1", "a0") -> 0.7)
    p += (("s1", "a1") -> 0.3)
    p += (("s1", "a2") -> 0.0)

    p += (("s2", "a0") -> 0.4)
    p += (("s2", "a1") -> 0.6)
    p += (("s2", "a2") -> 0.0)


    T += (("s0","a0","s0") -> (0.5,2.8))
    T += (("s0","a0","s1") -> (0.25,3.3))
    T += (("s0","a0","s2") -> (0.25,0.0))

    T += (("s0","a1","s0") -> (1.0,0.8))
    T += (("s0","a1","s1") -> (0.0,0.0))
    T += (("s0","a1","s2") -> (0.0,0.0))

    T += (("s0","a2","s0") -> (0.0,0.0))
    T += (("s0","a2","s1") -> (0.0,0.0))
    T += (("s0","a2","s2") -> (0.0,0.0))

    T += (("s1","a0","s0") -> (0.5,0.8))
    T += (("s1","a0","s1") -> (0.25,0.3))
    T += (("s1","a0","s2") -> (0.25,0.0))

    T += (("s1","a1","s0") -> (1.0,0.8))
    T += (("s1","a1","s1") -> (0.0,0.0))
    T += (("s1","a1","s2") -> (0.0,0.0))

    T += (("s1","a2","s0") -> (0.0,0.0))
    T += (("s1","a2","s1") -> (0.0,0.0))
    T += (("s1","a2","s2") -> (0.0,0.0))

    T += (("s2","a0","s0") -> (0.5,0.9))
    T += (("s2","a0","s1") -> (0.25,0.1))
    T += (("s2","a0","s2") -> (0.25,0.0))

    T += (("s2","a1","s0") -> (1.0,0.3))
    T += (("s2","a1","s1") -> (0.0,0.0))
    T += (("s2","a1","s2") -> (0.0,0.0))

    T += (("s2","a2","s0") -> (0.0,0.0))
    T += (("s2","a2","s1") -> (0.0,0.0))
    T += (("s2","a2","s2") -> (0.0,0.0))

  }

}