import scala.io.Source


class Crates(val arr: Array[Array[Char]]):
  def move(from: Int, to: Int, count: Int = 1): Crates =
    var arr_copy = arr
    arr_copy(to - 1) = arr_copy(to - 1) ++ arr_copy(from - 1).takeRight(count)
    arr_copy(from - 1) = arr_copy(from - 1).dropRight(count)
    Crates(arr_copy)
  
  def tops: String = arr.map(_.last).mkString
    

def read_and_store_crates: Crates =
  var arr: Array[Array[Char]] = Array.fill(9)(Array.empty)
  
  for line <- Source.fromFile("../05-input-crates.txt").getLines() do
    var x = 0;
    while x*4 + 1 < line.length do
      if line(x*4 + 1).isLetter then
        arr(x) = arr(x).prepended(line(x*4 + 1))
      x += 1
  
  Crates(arr)


def parse_move_line(line: String): (Int, Int, Int) =
  val split_line = line.split(' ')
  (split_line(1).toInt, split_line(3).toInt, split_line(5).toInt)


def read_moves_and_move(input_crates: Crates, by_one: Boolean = true): Crates =
  var crates = input_crates
  
  for line <- Source.fromFile("../05-input-moves.txt").getLines() do
    val (how_many, from, to) = parse_move_line(line)
    if by_one then
      for _ <- 0 until how_many do crates.move(from, to)
    else
      crates.move(from, to, how_many)
  crates


def task1: Unit = println(read_moves_and_move(read_and_store_crates).tops)
def task2: Unit =
  println(read_moves_and_move(read_and_store_crates, false).tops)


@main def day5 =
  task1
  task2