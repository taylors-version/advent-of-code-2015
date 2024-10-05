object Day23:
  private val input = io.Source.fromResource("Day23.txt").getLines().toSeq

  private val half = "hlf [ab]".r
  private val triple = "tpl [ab]".r
  private val increment = "inc [ab]".r
  private val jump = "jmp [+-]\\d+".r
  private val jumpEven = "jie [ab], [+-]\\d+".r
  private val jumpOne = "jio [ab], [+-]\\d+".r

  case class Registers(a: Long, b: Long)
  case class State(registers: Registers, position: Int):
    def process(): State = {
      val newState = if position >= input.length || position < 0 then copy()
      else {
        val instruction: String = input(position)
        instruction match
          case half() => Half.process(copy(), instruction).process()
          case triple() => Triple.process(copy(), instruction).process()
          case increment() => Increment.process(copy(), instruction).process()
          case jump() => Jump.process(copy(), instruction).process()
          case jumpEven() => JumpEven.process(copy(), instruction).process()
          case jumpOne() => JumpOne.process(copy(), instruction).process()
          case _ => copy()
      }
      val ben = 1
      newState
    }

  trait Instruction:
    def process(state: State, instruction: String): State

  case object Half extends Instruction {
    override def process(state: State, instruction: String): State = {
      val register = instruction(4)
      val registers = if register.equals('a') then state.registers.copy(a = state.registers.a / 2) else state.registers.copy(b = state.registers.b / 2)
      State(registers, state.position + 1)
    }
  }

  case object Triple extends Instruction{
    override def process(state: State, instruction: String): State = {
      val register = instruction(4)
      val registers = if register.equals('a') then state.registers.copy(a = state.registers.a * 3) else state.registers.copy(b = state.registers.b * 3)
      State(registers, state.position + 1)
    }
  }

  case object Increment extends Instruction{
    override def process(state: State, instruction: String): State = {
      val register = instruction(4)
      val registers = if register.equals('a') then state.registers.copy(a = state.registers.a + 1) else state.registers.copy(b = state.registers.b + 1)
      State(registers, state.position + 1)
    }
  }

  case object Jump extends Instruction{
    override def process(state: State, instruction: String): State = {
      state.copy(position = state.position + instruction.split(" ").last.toInt)
    }
  }

  case object JumpEven extends Instruction {
    override def process(state: State, instruction: String): State = {
      val registerVal: Long = if instruction(4).equals('a') then state.registers.a else state.registers.b
      state.copy(position = state.position + (if registerVal % 2 == 0 then instruction.split(" ").last.toInt else 1))
    }
  }

  case object JumpOne extends Instruction {
    override def process(state: State, instruction: String): State = {
      val registerVal: Long = if instruction(4).equals('a') then state.registers.a else state.registers.b
      state.copy(position = state.position + (if registerVal == 1 then instruction.split(" ").last.toInt else 1))
    }
  }

  def part1(): Long = {
    State(Registers(0L,0L), 0).process().registers.b
  }

  def part2(): Long = {
    State(Registers(1L, 0L), 0).process().registers.b
  }

  def main(args: Array[String]): Unit = {
    println(part1())
    println(part2())
  }
