object  Day03 :

    val directions = Map('^' -> (0,1), 'v' -> (0,-1), '>' -> (1,0), '<' -> (-1,0))

    case class Point(x: Int, y: Int):
        def delta(dx: Int, dy: Int) = Point(x + dx, y + dy)
        
    def followDirection(input: Seq[Char]): Seq[Point] = {
        input.scanLeft(Point(0,0))((current, next) =>  current.delta.tupled(directions(next)))
    }

    def part1(input: String): Int = {
        followDirection(input).distinct.size
    }

    def part2(input: String): Int = {
        val (santa, robot) = input.zipWithIndex.partitionMap((character, index) => if index % 2==0 then Left(character) else Right(character))
        (followDirection(santa) ++ followDirection(robot)).distinct.size
    }

    def main(args: Array[String]): Unit = {
        val data = io.Source.fromResource("Day03.txt").mkString
        println(part1(data))
        println(part2(data))
    }
