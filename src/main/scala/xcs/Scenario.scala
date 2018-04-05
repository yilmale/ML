package xcs

import scala.util.Random


/**
  * Created by Levent Yilmaz on 11/24/2017.
  */

object ScenarioType extends Enumeration {
  type ProblemType = Value
  val Classification, AdaptiveBehavior = Value

}

abstract class BaseScenario  {
  def conditionLength: Int
  def features: Array[Char]
  def current_situation: Condition
  def initial_training_cycles: Int
  def remaining_cycles: Int
  def actionTypes : Enumeration

  def createPopulation() : Set[XCSClassifierRule]

  def getPossibleActions : Set[Int]

  def sense(): Condition

  def correctAction() : Int

  def execute(act : Action): Option[Double]

  def more(): Boolean = {
    remaining_cycles > 0
  }

}


class Scenario extends BaseScenario {
  val conditionLength  = 4
  val features = Array[Char]('0', '1')
  var current_situation = null
  var initial_training_cycles = 1000
  var remaining_cycles = 1000

  object ActionTypes extends Enumeration {
    val A1 = Value("A1")
    val A2 = Value("A2")
    val A3 = Value("A3")
  }

  val actionTypes = ActionTypes

  def createPopulation() : Set[XCSClassifierRule] = {
    var P : Set[XCSClassifierRule] = Set()
    P
  }

  def getPossibleActions : Set[Int] = {
    ActionTypes.values map (c => {
      c.id
    })
  }

  def sense(): Condition = {
    var c = new Array[Char](conditionLength)
    for (i <- 0 until conditionLength) {
      c(i) = features(Random.nextInt(2))
    }
    var condition = new Condition(c)
    condition
  }

  def correctAction() : Int = {
    0
  }

  def execute(act : Action): Option[Double] = {
    remaining_cycles -= 1
    if (remaining_cycles >= 0) Some(0.5)
    else None
  }

}

class MUXProblem(cycles : Int) extends BaseScenario {
  val conditionLength  = 6
  val features = Array[Char]('0', '1')
  var current_situation : Condition = _
  var initial_training_cycles = cycles
  var remaining_cycles = cycles

  object ActionTypes extends Enumeration {
    val TRUE = Value("true")
    val FALSE = Value("false")

  }

  val actionTypes = ActionTypes

  def getPossibleActions : Set[Int] = {
    ActionTypes.values map (c => {
      c.id
    })
  }

  def createPopulation() : Set[XCSClassifierRule] = {
    var P: Set[XCSClassifierRule] = Set()
    for (i <- 0 until XCSConfiguration.max_population_size) {
      var c = new Array[Char](conditionLength)
      for (j <- 0 until 2) {
        var r2 = Random.nextDouble()
        if (r2 <= 0.5) c(j) = '0'
        else c(j) = '1'
      }
      for (j <- 2 until conditionLength) {
        var r1 = Random.nextDouble()
        if (r1 <= XCSConfiguration.wildcard_probability) c(j) = '#'
        else {
          var r2 = Random.nextDouble()
          if (r2 <= 0.5) c(j) = '0'
          else c(j) = '1'
        }
      }
      var cond = new Condition(c)
      var a = Random.nextInt(getPossibleActions.size)
      var action = new Action(a)
      var xcsRule = new XCSClassifierRule(cond, action, 0)
      P += xcsRule
    }
    P
  }

  def sense(): Condition = {
    var c = new Array[Char](conditionLength)
    for (i <- 0 until conditionLength) {
      c(i) = features(Random.nextInt(2))
    }
    var condition = new Condition(c)
    current_situation = condition
    current_situation
  }

  def correctAction() : Int = {
    var expected : Int = 0
    remaining_cycles = remaining_cycles - 1
    var addrBits = new Array[Int](2)
    if (current_situation.features(0)=='0') addrBits(0) = 0
    else addrBits(0)=1
    if (current_situation.features(1)=='1') addrBits(1) = 1
    else addrBits(1) = 0


    var index: Int = 2*addrBits(0)+addrBits(1)

    if (current_situation.features(2+index) == '0')
      expected = 0
    else
      expected = 1

    expected
  }

  def execute(act : Action): Option[Double] = {
    remaining_cycles -= 1
    var addrBits = new Array[Int](2)
    if (current_situation.features(0)=='0') addrBits(0) = 0
    else addrBits(0)=1
    if (current_situation.features(1)=='1') addrBits(1) = 0
    else addrBits(1) = 1

    var index: Int = 2*addrBits(0)+addrBits(1)
    var expected : Int = 0
    if (current_situation.features(2+index) == '0') expected = 0
    else if (current_situation.features(2+index) == '1') expected = 1
    else expected = -1
    if (expected == act.action) {
      Some(1.0)
    }
    else None
  }

}