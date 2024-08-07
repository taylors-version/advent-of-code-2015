object Day19:

  def part1(input: Seq[String]): Int = {
    val inputMolecule = input.last
    val transforms = input.dropRight(2)

    transforms.flatMap(t => {
      val splitT = t.split(" => ")
      inputMolecule.sliding(splitT.head.length).zipWithIndex.map((m, i) => {
        if(m.equals(splitT.head)) (inputMolecule.dropRight(inputMolecule.length - i) :+ splitT.last :+ inputMolecule.drop(splitT.head.length + i)).mkString
        else ""
      }).toSeq.filterNot(_.equals(""))
    }).distinct.size
  }

  def part2(input: Seq[String]): Int = {
    val molecule = input.last
    val elements = molecule.count(c => c.isUpper)
    val countRnAr = molecule.sliding(2).count(s => s.equals("Rn") || s.equals("Ar"))
    val countY = molecule.count(c => c=='Y')

    elements - countRnAr - countY * 2- 1
  }

  def main(args: Array[String]): Unit = {
    val data = io.Source.fromResource("Day19.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
  }
