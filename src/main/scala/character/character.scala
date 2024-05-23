package character
import scala.util.Random

trait Character:
    var name: String
    var life_points: Int
    var _type: String
    var _battleAction: String
    def gotDamaged(amount: Int): Unit
    def getDamage(): Int
    def isAlive(): Boolean

class Monster(var life_points: Int = 0, var name: String = "", var _type: String = "monster",
             var _battleAction: String = "") extends Character {


    override def gotDamaged(amount: Int): Unit = {
        life_points -= amount
    }

    override def getDamage(): Int = {
        2
    }

    override def isAlive(): Boolean = {
        return life_points > 0
    }
}

class Goblin(life_points: Int = 10, name: String = "Goblin") extends Monster(life_points, name)

class Player(var life_points: Int = 0, var name: String = "", var _type: String = "player",
              var _battleAction: String = "") extends Character {
    private var _pos: (Int, Int) = (0, 0)
    private val _rand = new scala.util.Random

    def isAlive(): Boolean = {
        return life_points > 0
    }

    override def gotDamaged(amount: Int): Unit = {
        life_points -= amount
    }

    def getPos(): (Int, Int) = {
        _pos
    }

    def setPos(pos: (Int, Int)): Unit = {
        _pos = pos
    }

    def move(x: Int, y: Int): Unit = {
        _pos = (_pos(0)+x, _pos(1)+y)
    }

    override def getDamage(): Int = {
        _rand.between(1, 10)
    }   
}