import play.api.libs.json.*
import play.api.libs.json.JsValue.jsValueToJsLookup

import scala.util.matching.Regex

object Day14:

    def part1(input: Seq[String], distance: Int): Int = {
        input.map(s => reindeerDistance(s.split(" "), distance)).max
    }

    def part2(input: Seq[String], distance: Int): Int = {
        Range.inclusive(1, distance).map(i => {
            val positions = input.map(s => reindeerDistance(s.split(" "), i))
            val max = positions.max
            positions.map(p => {
                if(p==max) 1
                else 0
            })
        }).foldLeft(Seq.fill(input.length)(0))((a, b) => (a zip b).map((x,y) => x+y)).max
    }

    def reindeerDistance(reindeer: Seq[String], distance: Int): Int ={
        val cycle: Int = reindeer(6).toInt + reindeer(13).toInt
        val multiplier = distance / cycle
        val remainder = distance - (multiplier * cycle)
        multiplier * reindeer(3).toInt * reindeer(6).toInt + reindeer(3).toInt * (reindeer(6).toInt min remainder)
    }


    def main(args: Array[String]): Unit = {
        val data = io.Source.fromResource("Day14.txt").getLines().toSeq
        println(part1(data, 2503))
        println(part2(data, 2503))
    }
