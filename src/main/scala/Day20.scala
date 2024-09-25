import scala.annotation.tailrec

object Day20:

  def part1(input: Long): Long = {
    getHouseBound(input)
  }

  def factorise(x: Long): List[Long] = {
    val smallDivisors = Range.inclusive(1, Math.sqrt(x).toInt).filter(i => x%i==0).map(_.toLong).toList
    val largeDivisors = smallDivisors.map(x/_)
    smallDivisors ++ largeDivisors
  }


  def parcelCount(houseFactors: List[Long]): Long = houseFactors.sum * 10

  @tailrec
  def getHouseBound(target: Long, houseNumber: Long = 1): Long = {
    if parcelCount(factorise(houseNumber)) < target then getHouseBound(target, houseNumber+1)
    else houseNumber

  }

  def main(args: Array[String]): Unit = {
    println(part1(34000000))
  }
