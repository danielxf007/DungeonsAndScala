package utils

class Point(var x: Int, var y: Int) {

    private var _coord = (x, y)

    def getCoord(): (Int, Int) = {
        _coord
    }
}
