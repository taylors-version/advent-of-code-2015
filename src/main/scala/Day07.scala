object Day07 {
    val and = ".*AND.*".r
    val or = ".*OR.*".r
    val not = "NOT.*".r
    val leftShift = ".*LSHIFT.*".r
    val rightShift = ".*RSHIFT.*".r

    def component(input: String): Int = input match
        case not() => {
            val inputs = input.split(" ")
            65535 - inputs(1).toInt
        }
        case and() => {
            val inputs = input.split(" ")
            inputs(0).toInt & inputs(2).toInt
        }
        case or() => {
            val inputs = input.split(" ")
            inputs(0).toInt | inputs(2).toInt
        }
        case leftShift() => {
            val inputs = input.split(" ")
            inputs(0).toInt << inputs(2).toInt
        }
        case rightShift() => {
            val inputs = input.split(" ")
            inputs(0).toInt >> inputs(2).toInt
        }
        case _ => input.toInt
}
