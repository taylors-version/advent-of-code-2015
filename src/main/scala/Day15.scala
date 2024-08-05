import play.api.libs.json.*
import play.api.libs.json.JsValue.jsValueToJsLookup

import scala.util.matching.Regex

object Day15:

    def part1(input: Seq[String]): Int = {
        sum100s.map(mixScore(input, _)).max
    }

    def part2(input: Seq[String]): Int = {
        sum100s.map(mixScoreWithCalories(input, _)).max
    }

    def mixScore(input: Seq[String], quantity: (Int, Int, Int, Int)): Int ={
        val frosting = input(0).replace(",", "").split(" ")
        val candy = input(1).replace(",", "").split(" ")
        val butterscotch = input(2).replace(",", "").split(" ")
        val sugar = input(3).replace(",", "").split(" ")

        val capacity = (frosting(2).toInt*quantity(0) + candy(2).toInt*quantity(1) + butterscotch(2).toInt*quantity(2) + sugar(2).toInt*quantity(3)) max 0
        val durability = (frosting(4).toInt*quantity(0) + candy(4).toInt*quantity(1) + butterscotch(4).toInt*quantity(2) + sugar(4).toInt*quantity(3)) max 0
        val flavor = (frosting(6).toInt*quantity(0) + candy(6).toInt*quantity(1) + butterscotch(6).toInt*quantity(2) + sugar(6).toInt*quantity(3)) max 0
        val texture = (frosting(8).toInt*quantity(0) + candy(8).toInt*quantity(1) + butterscotch(8).toInt*quantity(2) + sugar(8).toInt*quantity(3)) max 0

        capacity*durability*flavor*texture
    }

    def mixScoreWithCalories(input: Seq[String], quantity: (Int, Int, Int, Int)) = {
        val frosting = input(0).replace(",", "").split(" ")
        val candy = input(1).replace(",", "").split(" ")
        val butterscotch = input(2).replace(",", "").split(" ")
        val sugar = input(3).replace(",", "").split(" ")

        (frosting(10).toInt*quantity(0) + candy(10).toInt*quantity(1) + butterscotch(10).toInt*quantity(2) + sugar(10).toInt*quantity(3)) max 0 match
            case 500 => mixScore(input, quantity)
            case _ => 0
    }

    private def sum100s: Seq[(Int, Int, Int, Int)] = {
        Range.inclusive(0, 100).toList.flatMap(a => {
            Range.inclusive(0, 100 - a).toList.flatMap(b => {
                Range.inclusive(0, 100 - (a + b)).toList.flatMap(c => {
                    Range.inclusive(0, 100 - (a + b + c)).toList.map(d => {
                        (a, b, c, d)
                    })
                })
            })
        }).filter((a,b,c,d) => a+b+c+d == 100)
    }

    def main(args: Array[String]): Unit = {
        val data = io.Source.fromResource("Day15.txt").getLines().toSeq
        println(part1(data)) //18965440
        println(part2(data))
    }
