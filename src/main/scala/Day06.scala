object  Day06 :
    val on = "turn on.*".r
    val off = "turn off.*".r
    val toggle = "toggle.*".r

    def switch(line: String, p: Int): Int ={
        line match
            case on() => 1
            case off() => 0
            case toggle() => if p == 1 then 0 else 1
    }

    def dim(line: String, p: Int): Int ={
        line match
            case on() => p+1
            case off() => (p-1).max(0)
            case toggle() => p+2
    }

    def part1(input: String): Int = {
        val grid = Array.fill(1000)(Array.fill(1000)(0))
        
        input.split("\n").foreach{ line =>
            val Array(x1, y1, x2, y2) =  line.split("\\D+").tail.map(_.toInt)
            for x <- x1 to x2; y <- y1 to y2 do grid(x)(y) = switch(line, grid(x)(y))
        }
        grid.map(_.sum).sum
    }

    def part2(input: String): Int = {
        val grid = Array.fill(1000)(Array.fill(1000)(0))
        
        input.split("\n").foreach{ line =>
            val Array(x1, y1, x2, y2) =  line.split("\\D+").tail.map(_.toInt)
            for x <- x1 to x2; y <- y1 to y2 do grid(x)(y) = dim(line, grid(x)(y))
        }
        grid.map(_.sum).sum
    }

    def main(args: Array[String]): Unit = {
        val data = io.Source.fromResource("Day06.txt").mkString
        println(part1(data))
        println(part2(data))
    }
