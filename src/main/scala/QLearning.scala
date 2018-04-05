import breeze.linalg.argmax
import breeze.stats.distributions.{Rand, Uniform}


/**
  * Created by yilmaz on 7/26/17.
  */
object QLearning {
  var V = scala.collection.mutable.Map[String, Double]("s0" -> 0, "s1" -> 0, "s2" -> 0)
  var policy = scala.collection.mutable.Map[(String, String), Double]()
  var T = scala.collection.mutable.Map[(String, String, String), (Double, Double)]()
  val EpisodeLength : Int = 20
  val NS: Int = 3
  val S: List[String] = List("s0", "s1", "s2")
  val terminalState : String = "s2"
  val A: List[String] = List("a0", "a1", "a2")
  val learningRate : Double = 0.05
  val discount : Double = 0.1
  val EPISODES : Int = 5
  val epsilon : Double = 0.1


  var Q = scala.collection.mutable.Map[(String,String),Double]()

  val rf = (x : Tuple2[Tuple2[String,String], Double]) => {
    x._2
  }


  def apply(): Unit = {
    initialize()
    //    println("the Q values are " + Q)
    //    val m = argMax(Q, rf)
    //    println("Largest value is " + m)
    for (i <- 0 until EPISODES) {
      var state : String = "s0"
      do {
        var a : String = getAction(state,epsilon)
        println("Selected action is " + a)
        var R : Double = getReward(state,a)
        println("Getting next state from " + state + " and " + a)
        var nextState = getNextState(state,a)
        println("Next state is " + nextState)
        var q : scala.collection.mutable.Map[(String,String),Double] =
          Q filter (_._1._1 == nextState)
        println("Following Q values will be backed-up " + q)
        var bestA : String = argMax(q,rf).head._1._2
        Q((state,a)) += (learningRate * (R + (discount * Q(nextState,bestA)) - Q(state,a)))
        state = nextState

      } while (state != terminalState)
    }

    println("the Q values are " + Q)

  }

  def getAction(st: String, e: Double): String = {
    var ac : String = "a0"
    var ud = Uniform(0,1)
    var ui = Rand.randInt(0,S.length)
    if (ud.draw() <= epsilon)
      ac = A(ui.draw())
    else ac = argMax(Q,rf).head._1._2
    ac
  }

  def getReward(ns: String, na: String): Double = {
    var reward : Double = Rand.uniform.draw()
    reward
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


  def argMax[A, B: Ordering](input: Iterable[A], f: A => B) = {
    val fList = input map f
    val maxFList = fList.max
    input.view zip fList filter (_._2 == maxFList) map (_._1) toSet
  }

  def initialize(): Unit = {

    Q += (("s0","a0")->Rand.uniform.draw())
    Q += (("s0","a1")->Rand.uniform.draw())
    Q += (("s0","a2")->Rand.uniform.draw())

    Q += (("s1","a0")->Rand.uniform.draw())
    Q += (("s1","a1")->Rand.uniform.draw())
    Q += (("s1","a2")->Rand.uniform.draw())

    Q += (("s2","a0")->0.0)
    Q += (("s2","a1")->0.0)
    Q += (("s2","a2")->0.0)



    T += (("s0","a0","s0") -> (0.5,2.8))
    T += (("s0","a0","s1") -> (0.35,3.3))
    T += (("s0","a0","s2") -> (0.15,0.0))

    T += (("s0","a1","s0") -> (1.0,0.8))
    T += (("s0","a1","s1") -> (0.0,0.0))
    T += (("s0","a1","s2") -> (0.0,0.0))

    T += (("s0","a2","s0") -> (1.0,0.0))
    T += (("s0","a2","s1") -> (0.0,0.0))
    T += (("s0","a2","s2") -> (0.0,0.0))

    T += (("s1","a0","s0") -> (0.5,0.8))
    T += (("s1","a0","s1") -> (0.45,0.3))
    T += (("s1","a0","s2") -> (0.05,0.0))

    T += (("s1","a1","s0") -> (1.0,0.8))
    T += (("s1","a1","s1") -> (0.0,0.0))
    T += (("s1","a1","s2") -> (0.0,0.0))

    T += (("s1","a2","s0") -> (1.0,0.0))
    T += (("s1","a2","s1") -> (0.0,0.0))
    T += (("s1","a2","s2") -> (0.0,0.0))

    T += (("s2","a0","s0") -> (0.5,0.9))
    T += (("s2","a0","s1") -> (0.25,0.1))
    T += (("s2","a0","s2") -> (0.25,0.0))

    T += (("s2","a1","s0") -> (1.0,0.3))
    T += (("s2","a1","s1") -> (0.0,0.0))
    T += (("s2","a1","s2") -> (0.0,0.0))

    T += (("s2","a2","s0") -> (1.0,0.0))
    T += (("s2","a2","s1") -> (0.0,0.0))
    T += (("s2","a2","s2") -> (0.0,0.0))

  }

}