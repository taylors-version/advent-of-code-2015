object Day25:

  private val code: BigInt = 20151125
  private val base: BigInt = 252533
  private val modulo = 33554393

  def stepNumber(row: Int, column: Int): Int = {
    val triangleN = column + row - 1
    val triangle: Int = (triangleN * (triangleN + 1)) / 2
    triangle - (triangleN - column)
  }

  def part1(row: Int, column: Int): BigInt = {
    val exponent: Int = stepNumber(row, column)
    (base.modPow(exponent - 1, modulo) * code).mod(modulo)
  }

  def main(args: Array[String]): Unit = {
    val input = io.Source.fromResource("Day25.txt").getLines().toSeq.head.split(" ")
    println(part1(input(16).dropRight(1).toInt, input(18).dropRight(1).toInt))
  }
