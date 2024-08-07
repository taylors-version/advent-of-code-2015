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
                }).mkString
            })
            part1(newBoard, steps-1)
        }
    }

    def part2(input: Seq[String], steps: Int): Int = {
        val width = input.head.length - 1
        val height = input.length - 1
        val newInput = input.zipWithIndex.map((l,i) => {l.zipWithIndex.map( (c,j) => {
            if((i==0 && j==0) || (i==0 && j==width) || (i==height && j==0) || (i==height && j==width)) '#'
            else input(i)(j)
        }).mkString })
        if (steps < 1) newInput.map(_.count(c => c == '#')).sum
        else {
            val newBoard: Seq[String] = newInput.zipWithIndex.map((l, i) => {
                l.zipWithIndex.map((c, j) => {
                    toLive(newInput, Position(j, i))
                }).mkString
            })
            part2(newBoard, steps - 1)
        }
    }

    private def toLive(input: Seq[String], pos: Position): Character = {
        val countAlive = pos.neighbours.map(p => {
            if(p.x<0 || p.x >= input.head.length || p.y < 0 || p.y >= input.length)
                '.'
            else
              input(p.y)(p.x)
        }).count(_ =='#')

        if(countAlive == 3 || (countAlive == 2 && input(pos.y)(pos.x) == '#')) '#'
        else '.'
    }
    
    def main(args: Array[String]): Unit = {
        val data = io.Source.fromResource("Day18.txt").getLines().toSeq
        println(part1(data, 100))
        println(part2(data, 100))
    }
