package game

import character.*
import state.*
import stateMachine.*
import scala.collection.mutable.Map
import level.*



class Game {

    private var _player: Player = new Player()
    private var _goblin: Goblin = new Goblin(10)
    private var _gameStateMachine: StateMachine = new StateMachine()
    private var _rand = new scala.util.Random
    private var _level: Level = new Level()
    var _finished = false

    def setUpPlayer(life_points: Int, name: String): Unit = {
        _player.life_points = life_points
        _player.name = name
    }

    def getPlayer(): Player = {
        _player
    }

    def setUpLevel(): Unit = {
        _level.defineLayout()
    }

    def getLevel(): Level = {
        _level
    }

    def setUpGameStateMachine(): Unit = {
        var map: Map[String, State] = Map(
            "InitState" -> new InitState(this),
            "DungeonCreationState" -> new DungeonCreationState(this),
            "ExploringState" -> new ExploringState(this), 
            "BattlingState" -> new BattlingState(this),
            "DefeatedState" -> new DefeatedState(this))
        _gameStateMachine.init(map, "InitState")
    }

    def init(): Unit = {
        setUpGameStateMachine()
        _finished = false
    }

    def welcomePlayer(): Unit = {
        val json: ujson.Value  = ujson.read(os.read(os.pwd / "src" / "data" / "menu" / "main_menu.json"))
        println(json("welcome_mssg").str)
    }

    def game_loop(): Unit = {
        init()
        while(!(_finished) ){
            _gameStateMachine.execute()
        }


    }

}

