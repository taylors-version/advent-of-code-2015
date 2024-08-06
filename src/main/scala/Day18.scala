object  Day18 :
    case class Position(x: Int, y: Int) {
        lazy val neighbours: Seq[Position] = {
            (for{
                xNeighbour <- this.x - 1 to this.x + 1
                yNeighbour <- this.y - 1 to this.y + 1
            } yield Position(xNeighbour, yNeighbour)).filterNot(_ == this)
        }
    }

    def part1(input: Seq[String], steps: Int): Int = {
        if (steps < 1) input.map(_.count(c => c == '#')).sum
        else {
            val newBoard: Seq[String] = input.zipWithIndex.map((l, i) => {
                l.zipWithIndex.map((c, j) => {
                    toLive(input, Position(j, i))
                }).toString()
            })
            part1(newBoard, steps-1)
        }
    }

    private def toLive(input: Seq[String], pos: Position): Character = {
        val countAlive = (for{
            neighbour <- pos.neighbours
        }yield input(neighbour.y)(neighbour.x)).count(_=='#')

        if(countAlive == 3 || (countAlive == 2 && input(pos.y)(pos.x) == '#')) '#'
        else '.'
    }


    def main(args: Array[String]): Unit = {
        val data = io.Source.fromResource("Day18.txt").getLines().toSeq
        println(part1(data, 100))
    }
