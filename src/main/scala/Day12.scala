object  Day12 :

    def part1(input: String): Int =
        ("""-?\d+""".r findAllIn input).toList.map(_.toInt).sum


    def main(args: Array[String]): Unit = {
        val data = io.Source.fromResource("Day12.txt").mkString.trim
        println(part1(data))
    }
