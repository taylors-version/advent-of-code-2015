import scala.collection.mutable

object Day22:

  val MagicMissile: Spell = Spell(53, 4, 0, 0, 0, 0)
  val Drain: Spell = Spell(73, 2, 0, 2, 0, 0)
  val Shield: Spell = Spell(113, 0, 7, 0, 0, 6)
  val Poison: Spell = Spell(173, 3, 0, 0, 0, 6)
  val Recharge: Spell = Spell(229, 0, 0, 0, 101, 5)
  private val spells: Set[Spell] = Set(MagicMissile, Drain, Shield, Poison, Recharge)

  case class Boss(hp: Int, attack: Int)

  case class Spell(cost: Int, damage: Int, armor: Int, heal: Int, recharge: Int, duration: Int)

  case class State(bossHp: Int, bossAttack: Int, hp: Int, mana: Int, armor: Int, effects: Map[Spell, Int], var manaSpent: Int = 0, path: List[Spell], hardMode: Boolean = false):

    def personTurn(spell: Spell): State = {
      val afterEffects = applyEffects
      val afterHardMode = afterEffects.copy(hp = afterEffects.hp - (if hardMode then 1 else 0))
      val afterSpell = spell match
        case MagicMissile => afterHardMode.copy(bossHp = afterHardMode.bossHp - 4)
        case Drain => afterHardMode.copy(bossHp = afterHardMode.bossHp - 2, hp = afterHardMode.hp + 2)
        case _ => afterHardMode.copy(effects = afterHardMode.effects + ((spell, spell.duration)))
      afterSpell.copy(mana = afterSpell.mana - spell.cost, manaSpent = afterSpell.manaSpent + spell.cost, path = path.appended(spell))
    }

    def bossTurn(): State = {
      val afterEffects = applyEffects
      afterEffects.copy(hp = afterEffects.hp - (bossAttack-afterEffects.armor), armor = 0)
    }

    private def applyEffects: State = {
      effects.foldLeft(copy(armor = 0)){
        case (state, (spell, duration)) =>
          val step = effect(spell, state)
          if duration == 1 then step.copy(effects = state.effects.removed(spell))
          else step.copy(effects = state.effects.updated(spell, duration - 1))
      }
    }

    private def effect(spell: Spell, state: State): State = {
      spell match
        case Shield => state.copy(armor = 7)
        case Poison => state.copy(bossHp = state.bossHp - 3)
        case Recharge => state.copy(mana = state.mana + 101)
        case _ => state.copy()
    }

    def isGoal: Boolean = bossHp <= 0

    def next: Seq[State] = {
      if hp <= 0 then Seq.empty
      else {
        spells.diff(effects.filter((s,d) => d>1).keySet).filter(s => s.cost <= mana).map(spell => {
          personTurn(spell).bossTurn()
        }).toSeq
      }
    }


  private object StateOrdering extends Ordering[State] {
    def compare(a: State, b: State): Int = b.manaSpent compare a.manaSpent
  }


  def part1(boss: Boss): Int = {
    val startState: State = State(boss.hp, boss.attack, 50, 500, 0, Map.empty, 0, List.empty)
    dijkstra(startState).manaSpent
  }

  def part2(boss: Boss): Int = {
    val startState: State = State(boss.hp, boss.attack, 50, 500, 0, Map.empty, 0, List.empty, true)
    dijkstra(startState).manaSpent
  }

  private def dijkstra(startState: State): State = {
    val unvisitedStates = mutable.PriorityQueue[State]()(StateOrdering)
    unvisitedStates.enqueue(startState)

    while (unvisitedStates.nonEmpty) {
      val currentState = unvisitedStates.dequeue()
      if (currentState.isGoal) {
        return currentState
      }
      else
        currentState.next.foreach(s => unvisitedStates.enqueue(s))
    }
    startState.copy(manaSpent = Int.MaxValue)
  }


  def main(args: Array[String]): Unit = {
    val input = io.Source.fromResource("Day22.txt").getLines().toSeq
    val boss: Boss = Boss(input.head.split(" ").last.toInt, input.last.split(" ").last.toInt)
    println(part1(boss))
    println(part2(boss))
  }