object Day16:
  val required: Seq[String] = Seq("children: 3",
    "cats: 7",
    "samoyeds: 2",
    "pomeranians: 3",
    "akitas: 0",
    "vizslas: 0",
    "goldfish: 5",
    "trees: 3",
    "cars: 2",
    "perfumes: 1")

  def part1(input: Seq[String]): Int = input.indexWhere(meetsRequirements)+1

  def part2(input: Seq[String]): Int = input.indexWhere(meetsRetroRequirements)+1

  def meetsRequirements(sue: String): Boolean = {
    sue.replaceAll("Sue \\d+: ", "").split(", ").foldLeft(true)((a,b) => {
      required.find(s => s.startsWith(b.split(": ").head)).get.split(": ").last.equals(b.split(": ").last) && a
    })
  }

  def meetsRetroRequirements(sue: String): Boolean = {
    sue.replaceAll("Sue \\d+: ", "").split(", ").foldLeft(true)((a,b) => {
      val item = b.split(": ")
      item.head match
        case "cats" | "trees" => required.find(s => s.startsWith(item.head)).get.split(": ").last.toInt < item.last.toInt && a
        case "pomeranians" | "goldfish" => required.find(s => s.startsWith(item.head)).get.split(": ").last.toInt > item.last.toInt && a
        case _ => required.find(s => s.startsWith(item.head)).get.split(": ").last.toInt == item.last.toInt && a
    })
  }

  def main(args: Array[String]): Unit = {
    val data = io.Source.fromResource("Day16.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
  }
