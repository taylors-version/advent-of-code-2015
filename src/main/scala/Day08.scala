import java.util.regex.Pattern

object Day08 :

  def part1(input: Seq[String]): Int = {
    val regex: String = "\\x\\S\\S"
    val counts = input.map(s => s.length - escapedString(s).length)
    counts.sum
  }

  def escapedString(input: String): String = input.replace("\\\\", "b").replace("\\\"", "b").replaceAll("\\\\x..", "@").drop(1).dropRight(1)

  def main(args: Array[String]): Unit = {
    val data = io.Source.fromResource("Day08.txt").getLines().toSeq
    println(part1(data))
  }