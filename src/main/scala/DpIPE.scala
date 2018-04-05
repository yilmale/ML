import breeze.linalg.{DenseVector, _}
import breeze.stats.distributions.{Gaussian, Rand, Uniform}


object DpIPE {

  val NS: Int = 3
  val discount: Double = 0.1
  val S: List[String] = List("s0", "s1", "s2")
  val A: List[String] = List("a0", "a1", "a2")
  val threshold: Double = 0.001
  var delta: Double = 0
  var T = scala.collection.mutable.Map[(String, String, String), (Double, Double)]()
  var policy = scala.collection.mutable.Map[(String, String), Double]()
  var V = scala.collection.mutable.Map[String, Double]("s0" -> 0, "s1" -> 0, "s2" -> 0)

  def initializePolicy(p : scala.collection.mutable.Map[(String,String),Double],
                       T : scala.collection.mutable.Map[(String,String,String),(Double,Double)]): Unit = {
    p += (("s0","a0") -> 0.5)
    p += (("s0","a1") -> 0.3)
    p += (("s0","a2") -> 0.2)

    p += (("s1","a0") -> 0.7)
    p += (("s1","a1") -> 0.3)
    p += (("s1","a2") -> 0.0)

    p += (("s2","a0") -> 0.4)
    p += (("s2","a1") -> 0.3)
    p += (("s2","a2") -> 0.3)


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

  def getExpectedReward(s:String, a:String) : Double = {
    var r : Double = 0
    for (ns <- S) {
      var p = T.get((s,a,ns))
      r = r + ((p.get)._1 * ((p.get._2)+(discount*V(ns))))
    }
    r
  }

  def newStateValue(s: String): Double = {
    var v : Double = 0
    for (a <- A) {
      v += policy(s,a)*getExpectedReward(s,a)
    }
    v
  }

  def apply() {
    println("Initializing the policy and transition/reward probabilities")
    initializePolicy(policy, T)
    println(policy)
    do {
      delta = 0
      for (s <- S) {
        val v = V(s)
        V(s) = newStateValue(s)
        delta = Math.max(delta, Math.abs(v - V(s)))
      }
      println("delta value is " + delta)
    } while (delta >= threshold)
    println("Computed state value are")
    println()
    for (vs <- S) {
      println("State: "+ vs + " Value: " + V(vs))
    }
  }

}


object DpPI {
  val NS: Int = 3
  val discount: Double = 0.1
  val S: List[String] = List("s0", "s1", "s2")
  val A: List[String] = List("a0", "a1", "a2")
  val threshold: Double = 0.001
  var delta: Double = 0
  var T = scala.collection.mutable.Map[(String, String, String), (Double, Double)]()
  var pi = scala.collection.mutable.Map[String, String]("s0" -> "a0", "s1" -> "a1", "s2" -> "a2")
  var policy = scala.collection.mutable.Map[(String, String), Double]()
  var V = scala.collection.mutable.Map[String, Double]("s0" -> 0.0, "s1" -> 0.0, "s2" -> 0.0)

  def apply() {
    println("Initializing the policy and transition/reward probabilities")
    DpIPE.initializePolicy(policy, T)
    println(policy)

    println("Initial state values are")
    println()
    for (vs <- S) {
      println("State: "+ vs + " Value: " + V(vs))
    }

    do {
      delta = 0
      for (s <- S) {
        val v = V(s)
        V(s) = DpIPE.getExpectedReward(s,pi(s))
        delta = Math.max(delta, Math.abs(v - V(s)))
      }
      println("delta value is " + delta)
    } while (delta >= threshold)
    println("Computed state value are")
    println()
    for (vs <- S) {
      println("State: "+ vs + " Value: " + V(vs))
    }

    println("Starting policy improvement")
    var stable : Boolean = true
    do {
      stable = true
      for (s <- S) {
        var rf = getStateReward(s, _: String)
        var oldAction = pi(s)
        pi(s) = argMax(A, rf).head
        println("Old action is " + oldAction + " New action is " + pi(s) + "Difference is " + oldAction.compareTo(pi(s)))
        if (oldAction.compareTo(pi(s)) != 0)  stable =false
      }
    } while (stable == false)

    println("Policy improvement ends")
  }

  def argMax[A, B: Ordering](input: Iterable[A], f: A => B) = {
    val fList = input map f
    val maxFList = fList.max
    input.view zip fList filter (_._2 == maxFList) map (_._1) toSet
  }

  def getStateReward(s: String, act: String) : Double = {
    var r : Double = 0
    for (ns <- S) {
      var p = T.get((s,act,ns))
      r = r + ((p.get)._1 * ((p.get._2)+(discount*V(ns))))
    }
    r
  }




}