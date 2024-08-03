class Day13(data: Seq[String]) :

  def part1(): Int = people.permutations.toSeq.map(tableHappiness).max

  def part2(): Int = people.permutations.toSeq.map(tableWithMeHappiness).max

  private def people: Seq[String] = data.map(_.split(" ").head).distinct

  private def happiness(a: String, b: String): Int = {
    //Well this is hacky :/
    val line1 = data.find(l => l.split(" ").head.equals(a) && l.split(" ").last.equals(b+".")).getOrElse("a b me").split(" ")
    val line2 = data.find(l => l.split(" ").head.equals(b) && l.split(" ").last.equals(a+".")).getOrElse("a b me").split(" ")

    val happiness1 = line1(2) match
      case "me" => 0
      case "gain" => line1(3).toInt
      case "lose" => line1(3).toInt * -1

    val happiness2 = line2(2) match
      case "me" => 0
      case "gain" => line2(3).toInt
      case "lose" => line2(3).toInt * -1

    happiness1 + happiness2
  }

  private def tableHappiness(input: Seq[String]): Int = (input.sliding(2).toSeq :+ List(input.last, input.head)).map(i => happiness(i.head, i.last)).sum

  private def tableWithMeHappiness(input: Seq[String]): Int = (input.sliding(2).toSeq :+ List(input.last, "me") :+ List("me", input.head)).map(i => happiness(i.head, i.last)).sum
