import Day23.*
import org.scalatest.funsuite.AnyFunSuite


class Day23Test extends AnyFunSuite{
    val sampleState: State = State(Registers(6,10), 20)

    test("Half a") {
        val expected = sampleState.copy(registers = Registers(3,10), 21)
        assert(Half.process(sampleState, "hlf a").equals(expected))
    }

    test("Half b") {
        val expected = sampleState.copy(registers = Registers(6, 5), 21)
        assert(Half.process(sampleState, "hlf b").equals(expected))
    }

    test("Triple a") {
        val expected = sampleState.copy(registers = Registers(18, 10), 21)
        assert(Triple.process(sampleState, "tpl a").equals(expected))
    }

    test("Triple b") {
        val expected = sampleState.copy(registers = Registers(6, 30), 21)
        assert(Triple.process(sampleState, "tpl b").equals(expected))
    }

    test("Increment a") {
        val expected = sampleState.copy(registers = Registers(7, 10), 21)
        assert(Increment.process(sampleState, "inc a").equals(expected))
    }

    test("Increment b") {
        val expected = sampleState.copy(registers = Registers(6, 11), 21)
        assert(Increment.process(sampleState, "inc b").equals(expected))
    }

    test("Jump 5") {
        val expected = sampleState.copy(registers = Registers(6, 10), 25)
        assert(Jump.process(sampleState, "jmp +5").equals(expected))
    }

    test("Jump -7") {
        val expected = sampleState.copy(registers = Registers(6, 10), 13)
        assert(Jump.process(sampleState, "jmp -7").equals(expected))
    }

    test("Jump even a") {
        val expected = sampleState.copy(registers = Registers(6, 10), 25)
        assert(JumpEven.process(sampleState, "jie a, +5").equals(expected))
    }

    test("Jump even b") {
        val expected = sampleState.copy(registers = Registers(6, 10), 25)
        assert(JumpEven.process(sampleState, "jie b, +5").equals(expected))
    }

    test("Jump one a") {
        val state: State = State(Registers(1,10), 20)
        val expected = state.copy(registers = Registers(1, 10), 25)
        assert(JumpOne.process(state, "jio a, +5").equals(expected))
    }

    test("Jump one b") {
        val state: State = State(Registers(6, 1), 20)
        val expected = state.copy(registers = Registers(6, 1), 25)
        assert(JumpOne.process(state, "jio b, +5").equals(expected))
    }

}
