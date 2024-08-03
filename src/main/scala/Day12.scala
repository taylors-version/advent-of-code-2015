import scala.util.matching.Regex
import play.api.libs.json.*
import play.api.libs.json.JsValue.jsValueToJsLookup

object  Day12 :

    def part1(input: String): Int = ("""-?\d+""".r findAllIn input).toList.map(_.toInt).sum

    def part2(input: String): Int ={
        count2(Json.parse(input))
    }

    private def count2(v: JsValue): Int = v match {
        case JsNumber(x) => x.toInt
        case JsArray(a) => a.map(count2).sum
        case JsObject(o) if !(o.values.toList contains JsString("red")) => o.values.map(count2).sum
        case _ => 0
    }
    1120

    def main(args: Array[String]): Unit = {
        val data = io.Source.fromResource("Day12.txt").mkString.trim
        println(part1(data))
        println(part2(data)) //55739 too low
    }
