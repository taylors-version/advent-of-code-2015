import scala.annotation.tailrec

object Day20:

  def part1(input: Long): Long = {
    getHouseBound(input, 10)
  }

  def part2(input: Long): Long = {
    getHouseBound(input, 11, 50)
  }

  private def factorise(x: Long, maxHouses: Int): List[Long] = {
    val smallDivisors = Range.inclusive(1, Math.sqrt(x).toInt).filter(i => x%i==0).map(_.toLong).toList
    val largeDivisors = smallDivisors.map(x/_)
    (smallDivisors ++ largeDivisors).filter(f => x/f <= maxHouses)
  }


  private def parcelCount(houseFactors: List[Long], presents: Int): Long = {
    houseFactors.sum * presents
  }

  @tailrec
  private def getHouseBound(target: Long, presents: Int, maxHouses: Int = Int.MaxValue, houseNumber: Long = 1): Long = {
    if parcelCount(factorise(houseNumber, maxHouses), presents) < target then getHouseBound(target, presents, maxHouses, houseNumber+1)
    else houseNumber
  }

  def main(args: Array[String]): Unit = {
    println(part1(34000000))
    println(part2(34000000))
  }
