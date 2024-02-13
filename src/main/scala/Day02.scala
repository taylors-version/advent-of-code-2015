object  Day02 :

    def part1(input: Seq[String]): Int = input.map{line =>
        val Array(l,w,h) = line.split("x").map(_.toInt)
        val areas = Seq(l*w, w*h, h*l)

        areas.min + areas.sum * 2
    }.sum

    def part2(input: Seq[String]): Int = input.map{ line =>
        val Array(l,w,h) = line.split("x").map(_.toInt)
        val lengths = Seq(w+l, w+h, h+l)
        
        lengths.min * 2 + l*w*h
    }.sum

    def main(args: Array[String]): Unit = {
        val data = io.Source.fromResource("Day02.txt").getLines().toSeq
        println(part1(data))
        println(part2(data))
    }
