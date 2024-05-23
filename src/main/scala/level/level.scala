package level
import ujson.*
import scala.collection.mutable.Map
import scala.util.Random
import utils.*
import character.*

class Room(var roomType: String) {

    var _data: ujson.Value = ujson.read(os.read(os.pwd / "src" / "data" / "level" / "rooms.json"))
    
    def init(): Unit = {

    }

    def getDescription(): String = {
        _data(roomType)("Description").str
    }

    def getMonsters(): Array[Monster] = {
        Array()
    }

}

class EmptyRoom(roomType: String = "empty") extends Room(roomType)

class MonsterRoom(roomType: String = "monsters") extends Room(roomType) {

    private var _monsters: Array[Monster] = Array()
    private val _rand = new scala.util.Random

    def spawnMonster(name: String): Monster = {
        name match
            case "goblin" => Goblin(10)
    }

    def spawnMonsters(): Unit = {
        val monsters = _data(roomType)("monsterTypes").arr
        val minimun = _data(roomType)("minimun").num.toInt
        val maximun = _data(roomType)("maximun").num.toInt
        var nMonsters: Int = _rand.between(minimun, maximun)
        var monsterName: String = ""
        _monsters = Array()
        

        for( x <- 0 to nMonsters) {
            monsterName = monsters(0).str
            _monsters = _monsters :+ spawnMonster(monsterName)
        }

    }

    override def getMonsters(): Array[Monster] = {
        _monsters
    }

    override def init(): Unit = {
        spawnMonsters()
    }
}

class Level() {

    private val _directions: Array[String] = Array("right", "up", "left", "down")
    private val _room_types: Array[String] = Array("empty", "monsters")
    private val _rand = new scala.util.Random
    var _layout: Map[(Int, Int), Room] = Map.empty[(Int, Int), Room]

    def spawnRoom(roomType: String): Room = {
        roomType match
            case "empty" => new EmptyRoom()
            case "monsters" => new MonsterRoom()

    }

    def defineLayout(): Unit = {
        var nRooms = _rand.between(5, 12)
        var point = (0, 0)
        var direction: String = ""
        var x: Int = 0
        var y: Int = 0
        var room: Room = new Room("empty")
        var m: Int = 0
        _layout = Map.empty[(Int, Int), Room]
        _layout += (point -> new EmptyRoom())
        nRooms = nRooms - 1
        for (n <- 0 to nRooms) {
            direction = _directions(_rand.between(0, 3))
            direction match
                case "right" => {
                    point = (point(0)+1, point(1))
                }
                case "up" => {
                    point = (point(0), point(1)+1)
                }
                case "left" => {
                    point = (point(0)-1, point(1))
                }
                case "down" => {
                    point = (point(0), point(1)-1)
                }
            m = _rand.between(0, _room_types.length)
            room = spawnRoom(_room_types(m))
            room.init()
            _layout += (point -> room)
        }
    }

    def getRoom(pos: (Int, Int)): Room = {
        _layout(pos)            
    }

    def setRoom(pos: (Int, Int), newRoom: Room): Unit = {
        _layout -= pos
        _layout += (pos -> newRoom)            
    }

    def getMoveOptions(pos: (Int, Int)): Array[String] = {
        var options: Array[String] = Array()
        if (_layout.contains((pos(0)+1, pos(1)))) {
            options = options ++ Array("right")
        }
        if (_layout.contains((pos(0), pos(1)+1))) {
            options = options ++ Array("up")
        }
        if (_layout.contains((pos(0)-1, pos(1)))) {
            options = options ++ Array("left")
        }
        if (_layout.contains((pos(0), pos(1)-1))) {
            options = options ++ Array("down")
        }
        options
    }
}
