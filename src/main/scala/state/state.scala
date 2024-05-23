package state
import scala.collection.mutable.Map
import scala.io.StdIn.readLine
import game.*
import character.*
import level.*


abstract class State(_game: Game) {
    var _next: String = ""
    def start(): Unit
    def execute(): Unit
    def exit(): Unit
    def getNext(): String = {
        _next
    }
}

class InitState(_game: Game) extends State(_game) {
    var _name: String = "InitState"
    private val _data: ujson.Value = ujson.read(os.read(os.pwd / "src" / "data" / "menu" / "main_menu.json"))

    override def start() = {
        println(_data("welcome_mssg").str)
        _game.getPlayer().setPos((0,0))
    }

    override def exit(): Unit = {

    }

    override def execute() = {
        print("Enter your name: ")
        val name = readLine()
        _game.setUpPlayer(20, name)
        println(s"Let's start $name")
        _next = "DungeonCreationState"
    }
}

class DungeonCreationState(_game: Game) extends State(_game) {
    var _name: String = "DungeonCreationState"


    override def start() = {
        val _level: Level = _game.getLevel()
        _level.defineLayout()
    }

    override def exit(): Unit = {

    }

    override def execute() = {
        _next = "ExploringState"
    }

}

class ExploringState(_game: Game) extends State(_game) {
    var _name: String = "ExploringState"
    private val _moves = Array("right", "up", "left", "down") 
    private var _finished = false

    def getLevelRoom(): Room = {
        val _player: Player = _game.getPlayer()
        val _level: Level = _game.getLevel()
        _level.getRoom(_player.getPos())
    }
    
    override def start() = {
        val _room: Room = getLevelRoom()
        println(_room.getDescription())
        _finished = false
    }

    override def exit(): Unit = {

    }

    def getLevelMoveOptions(): Array[String] = {
        val _player: Player = _game.getPlayer()
        val _level: Level = _game.getLevel()
        _level.getMoveOptions(_player.getPos())
    }

    def printMoveOptions(): Unit = {
        val _player: Player = _game.getPlayer()
        val _level: Level = _game.getLevel()
        val options: Array[String] = _level.getMoveOptions(_player.getPos())
        print("You can move to: ")
        println(options.mkString(", "))
    }

    def processMoveChoice(choice: String): Unit = {
        val _player: Player = _game.getPlayer()
        val _roomBefore: Room = getLevelRoom()
        val _level: Level = _game.getLevel()
        val options: Array[String] = _level.getMoveOptions(_player.getPos())
        if (options.contains(choice)) {
            choice match
                case "right" => {
                    _player.move(1, 0)
                }
                case "up" => {
                    _player.move(0, 1)
                }
                case "left" => {
                    _player.move(-1, 0)
                }
                case "down" => {
                    _player.move(0, -1)
                }
            val _roomAfter: Room = getLevelRoom()
            println(_roomAfter.getDescription())
        }else {
            println("Invalid movement")
        }

    }

    def processChoice(choice: String): Unit = {
        if (_moves.contains(choice)) {
            processMoveChoice(choice)
        } else {
            println("Invalid choice")
        }
    }

    def enteredMonsterRoom(): Boolean = {
        val _room: Room = getLevelRoom()
        _room.roomType == "monsters"
    }

    override def execute() = {
        while( !_finished ){
            printMoveOptions()
            print("Enter your choice: ")
            val choice = readLine()
            processChoice(choice)
            if (enteredMonsterRoom()) {
                _next = "BattlingState"
                _finished = true
            }
        }
    }

}


class BattlingState(_game: Game) extends State(_game) {
    var _name: String = "BattlingState"
    private var _monsters: Array[Monster] = Array()
    private var _monsterIdx: Int = 0
    private var _player: Player = _game.getPlayer()
    private var _turn: String = "player"
    private val _rand = new scala.util.Random
    private var _finished = false


    def getLevelRoom(): Room = {
        val _level: Level = _game.getLevel()
        _level.getRoom(_player.getPos())
    }

    def printMonsters(): Unit = {
        println("You are against: ")
        for (n <- 0 to _monsters.length-1) {
            println(_monsters(n).name)
        }
    }
    
    override def start(): Unit = {
        val _room: Room = getLevelRoom()
        _monsters = _room.getMonsters()
        _monsterIdx = 0
        _turn = "player"
        _finished = false
        printMonsters()
    }


    def monsterTurn(): Unit = {
        val action: Int = _rand.between(0, 1)
        var damageAmount: Int = 0
        action match
            case 0 => {
                _monsters(_monsterIdx)._battleAction = "attack"
                damageAmount = _monsters(_monsterIdx).getDamage()
                if (_player._battleAction == "defend") {
                    damageAmount /= 2
                }
                _player.gotDamaged(damageAmount)
                println("--------------------------------------------")
                println(s"${_monsters(_monsterIdx).name} Attacked!")
                println(s"${_player.name} got damaged -$damageAmount")
                println(s"Life Points: ${_player.life_points}")
                if (!_player.isAlive()) {
                    println(s"${_player.name} died")
                }
                println("--------------------------------------------")
                println()
            }
            case 1 => {
                _monsters(_monsterIdx)._battleAction = "defend"
            }
        _monsterIdx = (_monsterIdx+1)%_monsters.length
    }

    def damageMonster(damage: Int, idx: Int = 0): Unit = {
        if (idx < _monsters.length) {
            if (_monsters(idx).isAlive()){
                var damageAmount: Int = damage
                val name: String = _monsters(idx).name
                if (_monsters(idx)._battleAction == "defend") {
                    damageAmount /= 2 
                }
                _monsters(idx).gotDamaged(damageAmount)
                println("--------------------------------------------")
                println(s"${_player.name} Attacked!")
                println(s"$name got damaged -$damageAmount")
                println(s"Life Points: ${_monsters(idx).life_points}")
                if (!_monsters(idx).isAlive()) {
                    println(s"${_monsters(idx).name} died")
                }
                println("--------------------------------------------")
            }else {
                damageMonster(damage, idx+1)
            }
        }

    }

    def processPlayerChoice(choice: String): Unit = {

        choice match
            case "attack" => {
                _player._battleAction = choice
                damageMonster(_player.getDamage())
            }
            case "defend" => {
                _player._battleAction = choice
            }
            case _ => {
                println("Invalid choice")
            }
    }    

    def playerTurn(): Unit = {
        print("attack or defend: ")
        val choice = readLine()
        processPlayerChoice(choice)
    }

    def monstersDefeated(): Boolean = {
        var defeated: Boolean = true
        for (n <- 0 to _monsters.length-1) {
            defeated = defeated && !(_monsters(n).isAlive())
        }
        defeated
    }

    def battleFinished(): Boolean = {
        monstersDefeated() || !(_player.isAlive())
    }

    override def exit(): Unit = {
        println("Battle Finished")
        _game.getLevel().setRoom(_player.getPos(), new EmptyRoom())
    }

    override def execute(): Unit = {
        val _player: Player = _game.getPlayer()
        while( !_finished ){
            if (_turn == "player") {
                playerTurn()
                _turn = "monster"
            }else {
                monsterTurn()
                _turn = "player"
            }
            _finished = battleFinished()
        }
        if(_player.isAlive()) {
            _next = "ExploringState"
        }else {
            _next = "DefeatedState"
        }
    }
}

class DefeatedState(_game: Game) extends State(_game) {
    private var _finished = false

    override def start(): Unit = {
        println("Game Over")
        _finished = false
    }

    override def execute(): Unit = {
        while (!_finished) {
            print("try again? (yes/no): ")
            val choice = readLine()
            choice match
                case "yes" => {
                    _next = "InitState"
                    _finished = true
                }
                case "no" => {
                    println("Good bye")
                    _finished = true
                    _game._finished = true

                }
                case _ => {
                    println("Invalid option")
                }
        }
    }

    override def exit(): Unit = {

    }

}