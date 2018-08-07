package exploratorySim

case class Parameter(name: String, var exists : Boolean, var value: Any)

class ModelSpace(ms : List[Parameter]) {
  var m : List[Parameter] = ms

}

case class Situation(name: String, var exists : Boolean, var value: Any)

class ContextSpace(cs: List[Situation]) {
  var c : List[Situation] = cs
}

class ScenarioSpace(ms: ModelSpace, cs: ContextSpace) {
  var m : ModelSpace = ms
  var s : ContextSpace = cs
}

class Ensemble {
  var scenarios : Set[ScenarioSpace] = null

}

object ExpSim {
 var  modelParameters = List(
   Parameter("InitSheep", true, 100),
   Parameter("InitWolf", true, 200),
   Parameter("SheepGain", true,4),
   Parameter("WolfGain", false, 0),
   Parameter("SheepReproduce", true, 4),
   Parameter("WolfReproduce", false, 5)
 )

}
