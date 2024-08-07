import scala.annotation.tailrec

object Day20:

  def part1(input: Long): Long = {
    //getHouseBound(input)
    println(factorise(1700864).distinct.sorted)
    
    0
  }

  @tailrec
  def factorise(x: Long, a: Long = 2, list: List[Long] = Nil): List[Long] = a * a > x match {
    case false if x % a == 0 => factorise(x / a, a, list ++ List[Long](a, x/a) )
    case false => factorise(x, a + 1, list)
    case true => x :: list
  }

  def parcelCount(houseFactors: List[Long]): Long = houseFactors.sum * 10

  @tailrec
  def getHouseBound(target: Long, houseNumber: Long = 1): Long = {
    if parcelCount((factorise(houseNumber) ++ List[Long](houseNumber, 1)).distinct) < target then getHouseBound(target, houseNumber+1)
    else houseNumber

  }

  def main(args: Array[String]): Unit = {
    println(part1(34000000)) // 1700864 too high
  }
