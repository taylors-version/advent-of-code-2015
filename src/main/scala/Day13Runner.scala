object Day13Runner {
  def main(args: Array[String]): Unit = {
    val data = io.Source.fromResource("Day13.txt").getLines().toSeq
    val day13 = new Day13(data)
    println(day13.part1())
    println(day13.part2())
  }
}

