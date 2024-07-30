import scala.collection.mutable

class Day07(data: Seq[String]) {
    private val and = ".*AND.*".r
    private val or = ".*OR.*".r
    private val not = "NOT.*".r
    private val leftShift = ".*LSHIFT.*".r
    private val rightShift = ".*RSHIFT.*".r
    private val nonInputs = Seq[String]("AND", "OR", "NOT", "LSHIFT", "RSHIFT", "->")

    def componentFunction(inputs: Seq[Int], command: String): Int = command match
        case not() => {
            65535 - inputs.head
        }
        case and() => {
            inputs(0) & inputs(1)
        }
        case or() => {
            inputs(0) | inputs(1)
        }
        case leftShift() => {
            inputs(0) << inputs(1)
        }
        case rightShift() => {
            inputs(0) >> inputs(1)
        }
        case _ => inputs.head

    def valueOfWire(wire: String): Int = {
        val a = data.filter( c => c.split(" -> ")(1).equals(wire)).head
        val inputs = inputsValue(a)
        componentFunction(inputs, a)
    }

    private def inputsValue(command: String): Seq[Int] = {
        val inputs = command.split(" -> ").head.split(" ").filterNot(nonInputs.contains(_))
        inputs.map(inputToInt)
    }

    private lazy val inputToInt: String => Int = memo {
        input =>
        if(input.forall(Character.isDigit)){
            input.toInt
        }else {
            valueOfWire(input)
        }
    }

    private def memo[K,V](f: K => V): K => V = new mutable.HashMap[K, V](){
        override def apply(key: K): V = getOrElseUpdate(key, f(key))
    }

    def part1(): Int ={
        valueOfWire("a")
    }
}
