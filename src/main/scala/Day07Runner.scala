object Day07Runner {
  def main(args: Array[String]): Unit = {
    val data = io.Source.fromResource("Day07.txt").getLines().toSeq
    val day07 = new Day07(data)
    val a = day07.part1()
    println(a)
    val part2Data = data.filterNot( c => c.split(" -> ")(1).equals("b")).appended(a + " -> b")
    val day07Part2 = new Day07(part2Data)
    println(day07Part2.part1())
  }
}

