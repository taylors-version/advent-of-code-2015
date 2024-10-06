import scala.annotation.tailrec

object Day24:

  def smallestGroup(input: Seq[Long], groupCount: Int = 3): Seq[Long] = {
    if groupCount <= 1 then input
    else {
      val targetWeight = input.sum / groupCount

      @tailrec
      def sequences(n: Int):Seq[Long]  = {
        val sequencesToSum = input.combinations(n).toSeq.filter(_.sum == targetWeight).filter(s =>
          smallestGroup(input.filterNot(s.toSet.contains), groupCount - 1).nonEmpty)
        if sequencesToSum.nonEmpty then sequencesToSum.minBy(_.product) else sequences(n+1)
      }
      sequences(1)
      }

  }


  def part1(input: Seq[Long]): Long = {
    val ben = smallestGroup(input)
    ben.product
  }

  def main(args: Array[String]): Unit = {
    val input = io.Source.fromResource("Day24.txt").getLines().toSeq.map(_.toLong)
    println(part1(input))//11846773891
  }
