import breeze.stats.distributions._


object MCPE {
  type T = scala.collection.mutable.ListBuffer[Double]
  val NS: Int = 3
  val discount: Double = 0.1
  val S: List[String] = List("s0", "s1", "s2")
  val A: List[String] = List("a0", "a1", "a2")
  val threshold: Double = 0.001
  var delta: Double = 0
  var T = scala.collection.mutable.Map[(String, String, String), (Double, Double)]()
  var policy = scala.collection.mutable.Map[(String, String), Double]()
  var V = scala.collection.mutable.Map[String, Double]("s0" -> 0, "s1" -> 0, "s2" -> 0)

  var Returns = scala.collection.mutable.Map[String, T](
    "s0" -> scala.collection.mutable.ListBuffer[Double](),
    "s1" -> scala.collection.mutable.ListBuffer[Double](),
    "s2" -> scala.collection.mutable.ListBuffer[Double]())



  val EpisodeLength : Int = 20

  def apply(): Unit = {
    initializePolicy(policy, T)
    var episode = generateEpisode()
    println(episode)
    var eList = episode.iterator
    while (eList.hasNext) {
      var nS = eList.next()
      if (eList.hasNext) {
        var nA = eList.next()
        val r = getReward(nS,nA)
        Returns(nS) += r
      }
    }
    println("Average return for each state is")

    for (s <- S) {
      println("Average for " + s + " is " + getAvg(Returns(s)))
    }

  }

  def getAvg(L : scala.collection.mutable.ListBuffer[Double]): Double = {
    var avg : Double = 0.0
    var sum : Double = 0.0
    var count : Int = L.size
    for (m <- L) {
      sum += m
    }
    avg = sum/count
    avg
  }

  def getReward(ns: String, na: String): Double = {
    var reward : Double = Rand.uniform.draw()
    reward
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

  def generateEpisode(): List[String] = {
    var ep = scala.collection.mutable.ListBuffer[String]()
    var curState : String = "s0"

    for (i <- 0 until EpisodeLength) {
      ep += curState
      val x : Double = Rand.uniform.draw()
      var selected : String = getSelectedAction(curState)
      ep += selected
      curState  = getNextState(curState,selected)
    }

    ep.toList
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