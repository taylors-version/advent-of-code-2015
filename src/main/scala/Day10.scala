object Day10 :

  def run(input: String, iterations: Int): Int = {
    repeated[String](s => lookAndSay(s), iterations)(input).length
  }

  def lookAndSay(input: String): String = {
    val ben = input.foldLeft(List.empty[List[String]]) {
      case (head :: tail, num) if head.head.head == num =>
        (num.toString +: head) :: tail
      case (result, num) =>
        List(num.toString) :: result
    }.reverse
    ben.foldLeft("") { (result, s) => result + s.length.toString + s.head }
  }

  private def repeated[A](f: A => A, n: Int): A => A =
    (0 until n).foldLeft(identity[A] _)((ff, _) => ff.andThen(f))

  def main(args: Array[String]): Unit = {
    val data = "1113122113"
    println(run(data, 40))
    println(run(data, 50))
  }