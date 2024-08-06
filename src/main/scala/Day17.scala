object Day17:

  def part1(input: Seq[String]): Int = allCombinations(input).count(_.sum == 150)

  def part2(input: Seq[String]): Int = {
    val matches = allCombinations(input).filter(_.sum == 150)
    matches.count(_.size == matches.minBy(_.size).size)
  }

  private def allCombinations(input: Seq[String]): IndexedSeq[Seq[Int]] = {
    Range(1, input.length).flatMap(input.zipWithIndex.combinations(_).toSeq.map(l => {
      l.map((a, b) => a.toInt)
    }))
  }

  def main(args: Array[String]): Unit = {
    val data = io.Source.fromResource("Day17.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
  }
