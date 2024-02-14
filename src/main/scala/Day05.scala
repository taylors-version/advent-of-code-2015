object  Day05 :

    val vowels = Seq('a', 'e', 'i', 'o', 'u')
    val forbidden = Seq("ab", "cd", "pq", "xy")

     def regex(patterns: String*): Seq[scala.util.matching.Regex] = patterns.map(_.r.unanchored)

    def contains3Vowels(input: String): Boolean = {
        input.filter(a => vowels.contains(a)).size > 2
    }

    def noForbidden(input: String): Boolean = {
        !forbidden.exists(input.contains)
    }

    def repeatedLetter(input: String): Boolean = {
        input.zipWithIndex.filter((character, index) => index>0 && character == input.charAt(index-1)).size > 0
    }

    def part1(input: String): Int = {
        input.split("\n").count(s => contains3Vowels(s) && noForbidden(s) && repeatedLetter(s))
    }

    def part2(input: String): Int = {
        val Seq(twoPair, triple) = regex("(..).*\\1", "(.).\\1")
        input.split("\n").count(s => twoPair.matches(s) && triple.matches(s))
    }

    def main(args: Array[String]): Unit = {
        val data = io.Source.fromResource("Day05.txt").mkString
        println(part1(data))
        println(part2(data))
    }
