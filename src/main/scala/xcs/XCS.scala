package xcs




import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Random
import ScenarioType._


/**
  * Created by Levent Yilmaz on 11/24/2017.
  */

class Condition(c : Array[Char]) {
  var features : Array[Char] = c
  override def toString : String  = {
    features foreach (x => {
      print(x + " ")

    })
    s" "
  }

}


class Action(a : Int) {
  var action : Int = a
}

object XCSConfiguration {
  val max_population_size = 50       // N
  val learning_rate = 0.15            // beta
  val accuracy_coefficient = 0.1      //alpha
  val error_threshold = 0.01          // epsilon_0
  val accuracy_power = 5              // nu
  val discount_factor = 0.71          // gamma
  val ga_threshold = 35               // theta_GA
  val crossover_probability = 0.75    // chi
  val mutation_probability = 0.03     // mu
  val deletion_threshold = 20         // theta_del
  val fitness_threshold = 0.1          // delta
  val subsumption_threshold = 20      // theta_sub
  val wildcard_probability = .33      // P_#
  val initial_prediction = .00001     // p_I
  val initial_error = 0.00001         // epsilon_I
  val initial_fitness = 0.0001       // F_I
  val exploration_probability = 0.5   //p_exp
  var minimum_actions : Int = 0          // theta_mna
  val do_ga_subsumption = false       // doGASubsumption
  val do_action_set_subsumption = false
  val pType : ProblemType = Classification
}

class XCSClassifierRule(cond : Condition, act : Action, ts : Int) {
  var rule : (Condition, Action) = (cond, act)
  var timeStamp = ts
  var averageReward : Double = XCSConfiguration.initial_prediction
  var error : Double = XCSConfiguration.initial_error
  var fitness : Double = XCSConfiguration.initial_fitness
  var experience : Int = 0
  var actionSetSize : Double = 1.0
  var numerosity : Int = 1
  var accuracy : Double = 0
  var correctTrack : Int = 0
  var correctSetSize : Double = 0


  def getCondition: Condition = {
    rule._1
  }

  def getAction : Action = {
    rule._2
  }

  override def toString : String = {
    val a : Int = act.action
    cond.toString + s"=> $a \n"+
      s"time: $timeStamp \n" +
      s"Reward:  $averageReward \n" +
      s"correctTrack: $correctTrack \n"  +
      s"accuracy: $accuracy \n"  +
      s"fitness: $fitness \n"  +
      s"experience: $experience \n" +
      s"numerosity: $numerosity \n" +
      s"action set size: $actionSetSize \n" +
      s"error: $error \n\n"

  }

}

class ClassifierSet(s : BaseScenario) {
  var population : Set[XCSClassifierRule] = Set()
  var scenario = s
  val cLength : Int = s.conditionLength
  XCSConfiguration.minimum_actions = scenario.getPossibleActions.size

  def init(): Unit = {
    for (i <- 0 until XCSConfiguration.max_population_size) {
      var c = new Array[Char](s.conditionLength)
      for (j <- 0 until s.conditionLength) {
        var r1 = Random.nextDouble()
        if (r1 <= XCSConfiguration.wildcard_probability) c(j)='#'
        else {
          var r2 = Random.nextDouble()
          if (r2 <= 0.5) c(j)='0'
          else c(j)='1'
        }
      }
      var cond = new Condition(c)
      var a = Random.nextInt(scenario.getPossibleActions.size)
      var action = new Action(a)
      var xcsRule = new XCSClassifierRule(cond,action,0)
      population += xcsRule
    }
  }

  def init(s : BaseScenario): Unit = {
    population = s.createPopulation()
  }


  def doesMatch(rule : Condition, situation : Condition) : Boolean = {
    var featureList = rule.features
    var i : Int = 0
    var matched : Boolean = true
    do {
      if ((featureList(i) != '#') && (featureList(i) != situation.features(i)))
        matched = false
      i = i + 1
    } while ((matched == true) && (i < featureList.length))
    matched
  }

  def countActions(rSet : Set[XCSClassifierRule]): scala.collection.mutable.Map[String,Int] = {
    var countMap = scala.collection.mutable.Map[String,Int]()
    var actionSet=scenario.getPossibleActions

    var it = actionSet.iterator
    while (it.hasNext) {
      var v = it.next()
      countMap += (scenario.actionTypes(v).toString -> 0)
    }

    var m = rSet.iterator
    while (m.hasNext) {
      var r = m.next()
      var a : Int = r.getAction.action
      var av = scenario.actionTypes(a)
      countMap(av.toString) += 1
    }

    countMap
  }

  def generateCoveringClassifier(ca: scala.collection.mutable.Map[String,Int], s : Condition) : XCSClassifierRule = {

    var c = new Array[Char](scenario.conditionLength)
    for (j <- 0 until 2) {
      c(j) = s.features(j)
    }
    for (j <- 2 until scenario.conditionLength) {
      var r1 = Random.nextDouble()
      if (r1 <= XCSConfiguration.wildcard_probability)
        c(j)='#'
      else
        c(j)= s.features(j)
    }
    var cnd = new Condition(c)


    var missingActions = new ArrayBuffer[String]()

    ca foreach (p => {if (p._2 == 0) missingActions += p._1})

    var selectedAction = missingActions(Random.nextInt(missingActions.length))
    var a = new Action(scenario.actionTypes.withName(selectedAction).id)

    var cl = new XCSClassifierRule(cnd,a,XCSAlgorithm.actualTime)
    cl

  }


  def generateMatchSet(s : Condition): Set[XCSClassifierRule] = {
    var M : Set[XCSClassifierRule]= Set()
    var cl : XCSClassifierRule = null
    while (M.isEmpty) {
      population foreach (r => {if (doesMatch(r.getCondition,s)) M += r})
      var CA = countActions(M)
      var actionCount : Int = 0
      (CA filter (p => {p._2 > 0})) foreach (f => {actionCount = actionCount + 1})
      if (actionCount < XCSConfiguration.minimum_actions) {
        cl = generateCoveringClassifier(CA,s)
        population += cl
        deleteFromPopulation()
        M = Set()
      }
    }
    M
  }

  def generatePredictionArray(M : Set[XCSClassifierRule]): Array[Double] = {
    var PA = new Array[Double](scenario.getPossibleActions.size)
    var FSA = new Array[Double](scenario.getPossibleActions.size)
    M foreach (cl => {
      PA(cl.getAction.action) += (cl.averageReward * cl.fitness)
      FSA(cl.getAction.action) += cl.fitness
    })

    for (i <- 0 until scenario.getPossibleActions.size) {
      if (FSA(i) != 0) PA(i) = PA(i) / FSA(i)
    }

    PA
  }

  def getMax(P: Array[Double]): Tuple2[Double, Int] = {
    var maxP : Double = P(0)
    var maxInd : Int = 0

    for (i <- 1 until P.length) {
      if (P(i) > maxP) {
        maxP = P(i)
        maxInd = i
      }
    }

    (maxP, maxInd)
  }

  def selectAction(PA : Array[Double]): Action = {
    var act : Int = 0
    var pred : Double = 0
    var selectedAction: Action = null
    if (Random.nextDouble() < XCSConfiguration.exploration_probability) {
      act = Random.nextInt(scenario.getPossibleActions.size)
    }
    else
    {
      var t = getMax(PA)
      act = t._2

    }
    selectedAction = new Action(act)
    selectedAction
  }

  def generateActionSet(M: Set[XCSClassifierRule], a : Action): Set[XCSClassifierRule] = {
    var A : Set[XCSClassifierRule]= Set()
    M foreach (cl => {
      if (cl.getAction.action == a.action) A += cl
    })
    A
  }

  def updateFitness(A: Set[XCSClassifierRule]): Unit = {
    var accuracySum : Double = 0
    var accuracyMap = scala.collection.mutable.Map[XCSClassifierRule,Double]()

    A foreach (cl => {
      if (cl.error < XCSConfiguration.error_threshold)
        accuracyMap += (cl -> 1.0)
      else {
        accuracyMap += (cl -> XCSConfiguration.accuracy_coefficient *
          (Math.pow(cl.error/XCSConfiguration.error_threshold,(0-XCSConfiguration.accuracy_power))))
      }
      accuracySum += accuracyMap(cl) * cl.numerosity
    })


    A foreach (cl => {
      cl.fitness += XCSConfiguration.learning_rate *
        (((accuracyMap(cl)*cl.numerosity)/accuracySum)-cl.fitness)
    })

  }

  def doActionSetSubsumption(A: Set[XCSClassifierRule]): Set[XCSClassifierRule] = {
    var cl: XCSClassifierRule = null
    var updatedA : Set[XCSClassifierRule] = null
    A foreach (c => {
      if (couldSubsume(c)) {
        if (((cl == null) || (countGenSymbol(c) > countGenSymbol(cl))) &&
          (((countGenSymbol(c) == countGenSymbol(cl))) && (Random.nextDouble() < 0.5)))
          cl =c
      }
    })

    if (cl != null) {
      A foreach (c => {
        if (isMoreGeneral(cl,c)) {
          cl.numerosity += c.numerosity
          updatedA = A - (c)
          population -= c
        }
      })
    }

    if (updatedA != null) updatedA
    else A

  }




  def updateActionSet(A: Set[XCSClassifierRule], r : Double): Set[XCSClassifierRule] = {
    var actionSetNumerosity : Int = 0
    var updatedA : Set[XCSClassifierRule] = null
    A foreach (cl => {actionSetNumerosity += cl.numerosity})
    A foreach (cl => {
      cl.experience += 1
      if (cl.experience < 1/XCSConfiguration.learning_rate) {
        cl.averageReward += (r-cl.averageReward)*(1/cl.experience.toDouble)
      }
      else {
        cl.averageReward += XCSConfiguration.learning_rate * (r - cl.averageReward)
      }

      if (cl.experience < 1/XCSConfiguration.learning_rate) {
        cl.error += (Math.abs(r - cl.averageReward) - cl.error) / (1 / cl.experience.toDouble)
      }
      else {
        cl.error += XCSConfiguration.learning_rate * (Math.abs(r - cl.averageReward) - cl.error)
      }
      if (cl.experience < 1/XCSConfiguration.learning_rate)
        cl.actionSetSize += (actionSetNumerosity - cl.actionSetSize) * (1/cl.experience.toDouble)
      else
        cl.actionSetSize += XCSConfiguration.learning_rate * (actionSetNumerosity - cl.actionSetSize)
    })

    updateFitness(A)



    if (XCSConfiguration.do_action_set_subsumption)
      updatedA = doActionSetSubsumption(A)

    if (updatedA != null)  updatedA
    else A
  }



  def deleteFromPopulation(): Unit = {

    def DeletionVote(c: XCSClassifierRule, avgFitness: Double): Double = {
      var vote : Double = c.actionSetSize*c.numerosity
      if ((c.experience>XCSConfiguration.deletion_threshold) &&
        ((c.fitness/c.numerosity.toDouble) > (XCSConfiguration.fitness_threshold*avgFitness))) {
        vote = vote * avgFitness / (c.fitness/c.numerosity.toDouble)
      }
      vote
    }

    var numberOfClassifiers: Int = 0
    var cumulativeFitness: Double = 0.0
    population foreach (cl => {
      numberOfClassifiers += cl.numerosity
      cumulativeFitness += cl.fitness
    })
    if (numberOfClassifiers > XCSConfiguration.max_population_size) {
      var avgFitnessInPopulation = cumulativeFitness / numberOfClassifiers
      var voteSum: Double = 0
      population foreach (cl => {
        voteSum += DeletionVote(cl, avgFitnessInPopulation)
      })
      var choicePoint = Random.nextDouble() * voteSum
      voteSum = 0
      var popIt = population.iterator
      var found: Boolean = false
      while ((popIt.hasNext) && (found == false)) {
        var c = popIt.next()
        voteSum += DeletionVote(c, avgFitnessInPopulation)
        if (voteSum > choicePoint) {
          if (c.numerosity > 1) c.numerosity -= 1
          else population -= c
          found = true
        }
      }
    }
  }

  def insertIntoPopulation(rule: XCSClassifierRule): Unit = {
    var i: Int = 0
    var found: Boolean = false
    var populationIt = population.iterator
    var cl: XCSClassifierRule = null

    while ((populationIt.hasNext) && (found==false))  {
      cl = populationIt.next()
      if ((compareCond(cl.getCondition,rule.getCondition)) &&
        (compareAction(cl.getAction,rule.getAction))) {
        found=true
        cl.numerosity += 1
      }
    }

    if (found == false) population += rule
  }

  def compareCond(c1: Condition, c2: Condition): Boolean = {
    var equal: Boolean = true
    var i : Int = 0
    while ((i < c1.features.length) && (equal)) {
      if (c1.features(i) != c2.features(i))
        equal = false
      i += 1
    }
    equal
  }

  def compareAction(a1: Action, a2: Action): Boolean = {
    var equal: Boolean = true
    if (a1.action != a2.action) equal=false
    equal
  }

  def couldSubsume(cl: XCSClassifierRule) : Boolean = {
    var acceptable: Boolean = false
    if (cl.experience > XCSConfiguration.subsumption_threshold) {
      if (cl.error < XCSConfiguration.error_threshold)
        acceptable = true
    }
    acceptable
  }

  def countGenSymbol(cl: XCSClassifierRule): Int = {
    var count : Int = 0
    if (cl != null) {
      cl.getCondition.features foreach (c => {
        if (c == '#') count += 1
      })
    }
    count
  }

  def isMoreGeneral(cgen: XCSClassifierRule, cspec: XCSClassifierRule) : Boolean = {
    var general : Boolean = true
    if (countGenSymbol(cgen) <= countGenSymbol(cspec))
      general = false

    if (general) {
      var i : Int = 0
      do {
        if ((cgen.getCondition.features(i) != '#') &&
          (cgen.getCondition.features(i) != cspec.getCondition.features(i)))
          general = false
        i += 1
      } while ((i < cgen.getCondition.features.length) && (general))
    }

    general
  }

  def subsumes(cgen: XCSClassifierRule, cspec: XCSClassifierRule) : Boolean = {
    var subsumes : Boolean = false
    if (cgen.getAction.action == cspec.getAction.action) {
      if (couldSubsume(cgen)) {
        if (isMoreGeneral(cgen,cspec)) subsumes=true
      }
    }
    subsumes
  }



  def runGA(A: Set[XCSClassifierRule], s : Condition): Unit = {

    def duplicate(p: XCSClassifierRule): XCSClassifierRule = {
      var cr = new XCSClassifierRule(p.getCondition,p.getAction,p.timeStamp)
      cr.averageReward = p.averageReward
      cr.fitness = p.fitness
      cr.numerosity = 1
      cr.experience = 0
      cr.actionSetSize = p.actionSetSize
      cr
    }

    def selectOffSpring(A: Set[XCSClassifierRule]): XCSClassifierRule = {
      var cl  : XCSClassifierRule = null
      var clP : XCSClassifierRule = null
      var fitnessSum : Double = 0

      var aclIt = A.iterator

      while (aclIt.hasNext) {
        cl = aclIt.next()
        fitnessSum += cl.fitness
      }


      var choicePoint = Random.nextDouble()*fitnessSum
      var found: Boolean = false
      fitnessSum = 0
      var AIter = A.iterator
      while ((AIter.hasNext) && (found == false)) {
        cl = AIter.next()
        fitnessSum += cl.fitness
        if (fitnessSum > choicePoint) {
          found = true
          clP = cl
        }
      }
      clP
    }

    def applyCrossOver(cl1: XCSClassifierRule, cl2: XCSClassifierRule): Unit = {
      var x : Int = Math.floor(Random.nextDouble() * (cl1.getCondition.features.length+1)).asInstanceOf[Int]
      var y : Int = Math.floor(Random.nextDouble() * (cl1.getCondition.features.length+1)).asInstanceOf[Int]

      if (x < y ) {
        var temp = x
        x = y
        y = temp
      }

      var i : Int = 0

      do {
        if ((x <= i) && (i < y)) {
          var temp = cl1.getCondition.features(i)
          cl1.getCondition.features(i) = cl2.getCondition.features(i)
          cl2.getCondition.features(i) = temp
        }
        i += 1
      } while (i < y)

    }

    def mutate(cl: XCSClassifierRule, s: Condition, lower: Int = 0, upper: Int = cLength): Unit = {
      var i : Int = lower

      do {
        if (Random.nextDouble() < XCSConfiguration.mutation_probability) {
          if (cl.getCondition.features(i) == '#')
            cl.getCondition.features(i) = s.features(i)
          else cl.getCondition.features(i) = '#'
        }
        i += 1
      } while (i < upper)

      if (Random.nextDouble() < XCSConfiguration.mutation_probability)
        cl.getAction.action  = Random.nextInt(scenario.getPossibleActions.size)
    }


    var cumulativeTS : Int = 0
    var numerosity : Int = 0
    A foreach (cl => {
      cumulativeTS += cl.timeStamp * cl.numerosity
      numerosity += cl.numerosity
    })

    if (XCSAlgorithm.actualTime - (cumulativeTS/numerosity) > XCSConfiguration.ga_threshold) {
      A foreach (cl => {cl.timeStamp = XCSAlgorithm.actualTime})
      var parent1 = selectOffSpring(A)
      var parent2 = selectOffSpring(A)
      var child1 = duplicate(parent1)
      var child2 = duplicate(parent2)
      var children = List(child1, child2)
      if (Random.nextDouble() < XCSConfiguration.crossover_probability) {
        applyCrossOver(child1, child2)
        child1.accuracy = (parent1.accuracy+parent2.accuracy)/2
        child1.correctTrack = (parent1.correctTrack+parent2.correctTrack)/2
        child1.averageReward = (parent1.averageReward+parent2.averageReward)/2
        child1.error = (parent1.error+parent2.error)/2
        child1.fitness = (parent1.fitness+parent2.fitness)/2
        child2.averageReward = child1.averageReward
        child2.error = child1.error
        child2.fitness = child1.fitness
      }
      else {
        child1.correctTrack = child1.correctTrack / 2
        child2.correctTrack = child2.correctTrack / 2
        child1.fitness = child1.fitness * 0.1
        child2.fitness = child2.fitness * 0.1
      }

      children foreach (c => {
        mutate(c, s, 2, scenario.conditionLength)
        if (XCSConfiguration.do_ga_subsumption) {
          if (subsumes(parent1,c))
            parent1.numerosity += 1
          else
          if (subsumes(parent2,c))
            parent2.numerosity += 1
          else insertIntoPopulation(c)
        }
        else insertIntoPopulation(c)

        deleteFromPopulation()
      })

    }
  }

  def insert(x: XCSClassifierRule, xs: List[XCSClassifierRule]) : List[XCSClassifierRule] =
    xs match {
      case List() => List(x)
      case y :: ys => if (x.fitness <= y.fitness) x :: xs
      else y :: insert(x, ys)
    }

  def iSort(L : List[XCSClassifierRule]) : List[XCSClassifierRule] =

    L match {
      case List() => List()
      case x :: xs1 => insert(x,iSort(xs1))
    }


  def sort() : List[XCSClassifierRule] = {
    var ruleBuffer: ListBuffer[XCSClassifierRule] = new ListBuffer[XCSClassifierRule]()
    population foreach (c => ruleBuffer += c )
    var ruleList = ruleBuffer.toList
    var sorted = iSort(ruleList)
    sorted
  }

  def UCS(scenario: BaseScenario): Unit = {

    do {
      var M : Set[XCSClassifierRule] = Set()
      var C : Set[XCSClassifierRule] = Set()
      var situation: Condition = scenario.sense()
      var correctAction = scenario.correctAction()
      XCSAlgorithm.actualTime += 1
      population foreach (c => {
        if (doesMatch(c.getCondition, situation))
          M += c
      })

      var matchIt = M.iterator
      var found: Boolean = false
      while ((matchIt.hasNext) && (found == false)) {
        var r = matchIt.next()
        if (r.getAction.action == correctAction)
          found = true
      }

      if (found == false) {
        var c = new Array[Char](scenario.conditionLength)
        var s = scenario.current_situation
        for (j <- 0 until 2) {
          c(j) = s.features(j)
        }
        for (k <- 2 until scenario.conditionLength) {
          if (Random.nextDouble() <= XCSConfiguration.wildcard_probability)
            c(k) = '#'
          else
            c(k) = s.features(k)
        }
        var cnd = new Condition(c)
        var a = new Action(correctAction)
        var cl = new XCSClassifierRule(cnd, a, XCSAlgorithm.actualTime)

        population += cl
        M += cl
      }

      M foreach (cl => {
        cl.experience += 1
        if (cl.getAction.action == correctAction)
          C += cl
      })
      var numerositySum: Int = 0
      C foreach (c => {
        c.correctTrack += 1
        numerositySum += c.numerosity
      })

      M foreach (cl => {
        cl.accuracy = cl.correctTrack / cl.experience.toDouble
        cl.fitness = Math.pow(cl.accuracy, XCSConfiguration.accuracy_power)
        if (cl.experience < (1 / XCSConfiguration.learning_rate))
          cl.correctSetSize *= ((numerositySum * (cl.experience - 1)) / (cl.experience.toDouble))
        else
          cl.correctSetSize += XCSConfiguration.learning_rate * (numerositySum - cl.correctSetSize)
      })

      runGA(C, situation)
    } while (scenario.more())
  }

  def run(scenario: BaseScenario): Unit = {
    if (XCSConfiguration.pType == Classification) UCS(scenario)
    else XCS(scenario)
  }


  def XCS(scenario : BaseScenario): Unit = {
    var pastReward : Double = 0
    var M : Set[XCSClassifierRule] = Set()
    var A : Set[XCSClassifierRule] = Set()
    var APrev: Set[XCSClassifierRule] = Set()
    var PA : Array[Double] = null
    var act : Action = null
    var immediateReward : Double = 0
    var reward: Double = 0
    var situation : Condition = null
    var prevSituation : Condition = null
    do {
      XCSAlgorithm.actualTime += 1
      situation = scenario.sense()
      M = generateMatchSet(situation)
      PA = generatePredictionArray(M)
      act = selectAction(PA)
      A = generateActionSet(M,act)
      immediateReward = scenario.execute(act).getOrElse(0)
      if (APrev.nonEmpty) {
        if (XCSConfiguration.pType == Classification)
          reward = pastReward
        else reward = pastReward + XCSConfiguration.discount_factor*getMax(PA)._1
        APrev = updateActionSet(APrev,reward)
        runGA(APrev,prevSituation)
      }

      if (scenario.remaining_cycles == 0) {
        reward = immediateReward
        A = updateActionSet(A,reward)
        runGA(A,situation)
        APrev = Set()
      }
      else {
        APrev = A
        pastReward = immediateReward
        prevSituation = situation
      }

    } while (scenario.more())

  }

}



object XCSAlgorithm {
  var model : ClassifierSet = _
  var scenario : BaseScenario = _
  var actualTime : Int = 0
  def apply(s : BaseScenario): Unit = {
    scenario = s
    model = initializeXCS(scenario)
    model.run(scenario)

  }

  def initializeScenario(): BaseScenario = {
    var e = new MUXProblem(5000)
    e
  }

  def initializeXCS(s: BaseScenario): ClassifierSet = {
    var classifierSet = new ClassifierSet(s)
    //classifierSet.init()
    classifierSet.init(s)
    classifierSet
  }

}