package stateMachine
import state.*
import scala.collection.mutable.Map


class StateMachine() {

    var _states: Map[String, State] = Map.empty[String, State]
    var _current: String = ""

    def init(states: Map[String, State], initial: String): Unit = {
        _states = states
        _current = initial
    }

    def execute(): Unit = {
        var _state: State = _states(_current)
        _state.start()
        _state.execute()
        _state.exit()
        update(_state.getNext())
    }

    def update(nextState: String): Unit = {
        _current = nextState
    }
}