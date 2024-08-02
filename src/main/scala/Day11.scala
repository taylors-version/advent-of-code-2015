object Day11:

  def password(input: String): String = {
    val potential = nextPotentialPassword(input)
    if(iolTest(potential) && doubleTest(potential) && sequentialTest(potential))
      potential
    else
      password(potential)
  }

  def iolTest(input: String): Boolean = !input.exists(c => c == 'i' || c == 'o' || c == 'l')

  def doubleTest(input: String): Boolean = input.sliding(2).filter(s => s(0).equals(s(1))).distinct.size > 1

  def sequentialTest(input: String): Boolean =
    input.sliding(3).count(s => s(1).equals(nextAlphaLetter(s(0))) && s(2).equals(nextAlphaLetter(s(1)))) > 0

  def nextAlphaLetter(char: Character): Character = (char+1).toChar

  def nextPotentialPassword(input: String): String = {
    input.reverse.segmentLength(_=='z', 0) match
      case 0 => input.init + nextPasswordLetter(input.last)
      case z => {
        val letterToChange = input.length - (z+1)
        input.substring(0, letterToChange) + nextPasswordLetter(input(letterToChange)) + "a" * z
      }
  }

  def nextPasswordLetter(char: Character): Character = { char match
    case 'h' => 'j'
    case 'k' => 'm'
    case 'n' => 'p'
    case 'z' => 'a'
    case c => (c+1).toChar
  }

  def main(args: Array[String]): Unit = {
    val data = "vzbxkghb"
    val part1 = password(data)
    println(part1)
    println(password(part1))
  }