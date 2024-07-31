class Day09(data: Seq[String]) :

  private val routes = possibleRoutes()

  def part1(): Int = {
    routes.min
  }

  def part2(): Int = {
    routes.max
  }

  private def possibleRoutes(): Seq[Int] = {
    locations.permutations.toSeq.map(distances)
  }

  private def locations: Seq[String] = {
    data.map(_.split(" ").head).distinct :+ data.last.split(" ")(2)
  }

  def distance(a: String, b: String): Int = {
    data.find(hasBothLocations(_, a, b)).get.split(" ")(4).toInt
  }

  private def hasBothLocations(edge: String, a: String, b: String): Boolean = {
    val inputSplit = edge.split(" ")
    (inputSplit(0).equals(a) && inputSplit(2).equals(b)) || (inputSplit(0).equals(b) && inputSplit(2).equals(a))
  }

  private def distances(input: Seq[String]): Int ={
    input.sliding(2).toSeq.map(i => distance(i.head, i.last)).sum
  }
