object Day09Runner {
  def main(args: Array[String]): Unit = {
    val data = io.Source.fromResource("Day09.txt").getLines().toSeq
    val day09 = new Day09(data)
    println(day09.part1())
    println(day09.part2())
  }
}

