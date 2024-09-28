object Day21 :

  val items: Seq[Item] = Seq(Item("weapon", "dagger", 8, 4, 0),
    Item("weapon", "shortsword", 10, 5, 0),
    Item("weapon", "warhammer", 25, 6, 0),
    Item("weapon", "longsword", 40, 7, 0),
    Item("weapon", "greataxe", 74, 8, 0),
    Item("armor", "leather", 13, 0, 1),
    Item("armor", "chainmail", 31, 0, 2),
    Item("armor", "splintmail", 53, 0, 3),
    Item("armor", "bandedmail", 75, 0, 4),
    Item("armor", "platemail", 102, 0, 5),
    Item("ring", "d1", 25, 1, 0),
    Item("ring", "d2", 50, 2, 0),
    Item("ring", "d3", 100, 3, 0),
    Item("ring", "a1", 20, 0, 1),
    Item("ring", "a2", 40, 0, 2),
    Item("ring", "a3", 80, 0, 3))

  case class Boss(hp: Int, attack: Int, armor: Int)
  case class Item(itemType: String, name: String, cost: Int, damage: Int, armor: Int)

  def part1(boss: Boss): Int = {
    listCost(possItems().filter(doesWin(_, boss, 100)).sorted(ItemOrdering).head)
  }

  def part2(boss: Boss): Int = {
    listCost(possItems().filter(!doesWin(_, boss, 100)).sorted(ItemOrdering).last)
  }

  def doesWin(items: Seq[Item], boss: Boss, health: Int): Boolean = {
    val attackStrength = items.foldLeft(0)((x,y) => x + y.damage)
    val youAttack: Double = (attackStrength - boss.armor).max(1)
    val armorStrength = items.foldLeft(0)((x, y) => x + y.armor)
    val bossAttack: Double = (boss.attack - armorStrength).max(1)

    val attacksToKillBoss: Int = math.ceil(boss.hp/youAttack).toInt
    val attacksToKillMe: Int = math.ceil(health/bossAttack).toInt

    attacksToKillBoss<=attacksToKillMe
  }

  object ItemOrdering extends Ordering[Seq[Item]] {
    def compare(a: Seq[Item], b: Seq[Item]): Int = listCost(a) compare listCost(b)
  }

  def listCost(items: Seq[Item]): Int = {
    items.foldLeft(0)((x,y) => x + y.cost)
  }

  def possItems(): Seq[Seq[Item]] = {
    (items.combinations(1) ++ items.combinations(2) ++ items.combinations(3) ++ items.combinations(4)).filter(allowedCombination).toSeq
  }

  def allowedCombination(items: Seq[Item]): Boolean = {
    val weaponCount = items.count(_.itemType.equals("weapon"))
    val armorCount = items.count(_.itemType.equals("armor"))
    val ringCount = items.count(_.itemType.equals("ring"))

    weaponCount == 1 && armorCount <=1 && ringCount <=2
  }

  def main(args: Array[String]): Unit = {
    val input = io.Source.fromResource("Day21.txt").getLines().toSeq
    val boss: Boss = Boss(input(0).split(" ").last.toInt, input(1).split(" ").last.toInt, input(2).split(" ").last.toInt)
    println(part1(boss))
    println(part2(boss))
  }