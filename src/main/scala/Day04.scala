import java.security.MessageDigest
object  Day04 :

    private def md5(inputStr: String): String = {
        val md: MessageDigest = MessageDigest.getInstance("MD5")
        md.digest(inputStr.getBytes()).map(0xFF & _).map { "%02x".format(_) }.foldLeft("") {_ + _}
    }



    def part1(input: String): Int = {
        Iterator.from(0).indexWhere(index => md5(input + index).startsWith("00000"))
    }

    def part2(input: String): Int = {
        Iterator.from(0).indexWhere(index => md5(input + index).startsWith("000000"))
    }

    def main(args: Array[String]): Unit = {
        val data = io.Source.fromResource("Day04.txt").mkString
        println(part1(data))
        println(part2(data))
    }
