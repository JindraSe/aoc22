import scala.io.Source


class Crates(val vec: Vector[Vector[Char]]):
  def move(from: Int, to: Int, count: Int, reversed: Boolean = true): Crates =
    val maybe_reverse = (v: Vector[Char]) => if reversed then v.reverse else v
    
    Crates(
      vec.updated(to - 1,
        vec(to - 1) ++ maybe_reverse(vec(from - 1).takeRight(count))
      ).updated(from - 1,
        vec(from - 1).dropRight(count)))

  def tops: String = vec.map(_.last).mkString


def read_and_store_crates: Crates =
  Crates(Source.fromFile("../05-input-crates.txt").getLines(
    ).filter(
      _.trim.nonEmpty
    ).foldLeft(
      Vector.fill(9)(Vector.empty)
    )(
      (vec: Vector[Vector[Char]], line: String) =>
        vec.zipWithIndex.map(
          (col, i) =>
            val ch = line(i*4 + 1)
            if ch.isLetter then ch +: col else col
        )
    ))
  

def parse_move_line(line: String): (Int, Int, Int) =
  val split_line = line.split(' ')
  (split_line(1).toInt, split_line(3).toInt, split_line(5).toInt)


def read_moves_and_move(input_crates: Crates, by_one: Boolean = true): Crates =
  Source.fromFile("../05-input-moves.txt").getLines.foldLeft(input_crates)(
    (crates: Crates, line: String) =>
      val (how_many, from, to) = parse_move_line(line)
      crates.move(from, to, how_many, by_one)
  )


def task1: Unit =
  println(read_moves_and_move(read_and_store_crates).tops)


def task2: Unit =
  println(read_moves_and_move(read_and_store_crates, false).tops)


@main def day5 =
  task1
  task2
